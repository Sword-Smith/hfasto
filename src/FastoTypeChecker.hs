module FastoTypeChecker where

import Fasto

import Control.Monad
import Data.List as L

data Env = Env { allFuns :: [FunDec]
               , prevFuns :: [ID]
               , vars :: [Param]
               }

failWith :: String -> Either String a
failWith = Left

check :: Prog -> Either String ()
check prog = void $ checkProg env prog
  where
    env = Env { allFuns = prog
              , prevFuns = []
              , vars = []
              }

checkProg :: Env -> Prog -> Either String Env
checkProg = foldM checkFun

-- Function names should occur only once. [x]
-- Function body should match return type. [x]
-- Params should be added to Env and have unique names. [ ]
checkFun :: Env -> FunDec -> Either String Env
checkFun env (Fun funTy name params bodyExp) =
  do env' <- checkUniqueName name env
     bodyTy <- checkExp env' bodyExp
     if funTy == bodyTy
       then return env'
       else failWith $ "Function '" ++ name ++ "' has type '" ++ show funTy ++
                       "' but its body has type '" ++ show bodyTy ++ "'"

  where
    checkUniqueName :: ID -> Env -> Either String Env
    checkUniqueName name env =
      if name `elem` prevFuns env
      then failWith $ "Function name '" ++ name ++ "' occurred more than once."
      else return env { prevFuns = name : prevFuns env }

-- Variable names should be defined previously. [ ]
-- Operator operands should match in type. [ ]
checkExp :: Env -> Exp -> Either String Type
checkExp env exp =
  case exp of
    ID id -> checkValidID env id
    Literal (IntVal _) -> return Int
    Literal (CharVal _) -> return Char
    Literal (BoolVal _) -> return Bool
    PlusExp exp1 exp2 -> checkBinop env "+" Int exp1 exp2
    MinusExp exp1 exp2 -> checkBinop env "-" Int exp1 exp2
    MultExp exp1 exp2 -> checkBinop env "*" Int exp1 exp2
    DivExp exp1 exp2 -> checkBinop env "/" Int exp1 exp2
    EqExp exp1 exp2 -> checkEq env exp1 exp2
    LtExp exp1 exp2 -> checkBinop env "<" Int exp1 exp2
    AndExp exp1 exp2 -> checkBinop env "&&" Bool exp1 exp2
    OrExp exp1 exp2 -> checkBinop env "||" Bool exp1 exp2
    NegateExp exp1 -> checkUnop env "!" Bool exp1
    NotExp exp1 -> checkUnop env "~" Int exp1
    IfExp exp1 exp2 exp3 -> checkIfExp env exp1 exp2 exp3
    LetExp name exp expBody -> do
      ty <- checkExp env exp
      let env' = env { vars = (ty,name) : vars env }
      checkExp env' expBody

checkIfExp :: Env -> Exp -> Exp -> Exp -> Either String Type
checkIfExp env exp1 exp2 exp3 = do
  ty1 <- checkExp env exp1
  ty2 <- checkExp env exp2
  ty3 <- checkExp env exp3
  when (ty1 /= Bool) $ failWith $ "First expression in if must return a boolean. Got " ++ show ty1
  when (ty2 /= ty3) $ failWith $ "\"then\" and else part must return same type. Got " ++ show ty2 ++ " and " ++ show ty3 ++ "."
  return ty2

checkBinop :: Env -> String -> Type -> Exp -> Exp -> Either String Type
checkBinop env op ty exp1 exp2 = do
  ty1 <- checkExp env exp1
  ty2 <- checkExp env exp2
  if ty1 == ty && ty2 == ty
    then return ty
    else failWith $ "Operator '" ++ op ++ "' operands have type " ++ show ty1
                    ++ " and " ++ show ty2 ++ ", expected " ++ show ty ++ "."

checkUnop :: Env -> String -> Type -> Exp -> Either String Type
checkUnop env op ty exp1 = do
  ty1 <- checkExp env exp1
  if ty1 == ty
     then return ty
    else failWith $ "Operator '" ++ op ++ "' operand has type " ++ show ty1
                    ++ ", expected " ++ show ty ++ "."

checkEq :: Env -> Exp -> Exp -> Either String Type
checkEq env exp1 exp2 = do
  ty1 <- checkExp env exp1
  ty2 <- checkExp env exp2
  if (ty1 == ty2)
    then return ty1
    else failWith $ "Operator '==' operands have type " ++ show ty1
                    ++ " and " ++ show ty2 ++ ", expected equal types."

checkValidID :: Env -> ID -> Either String Type
checkValidID env name =
  case L.find (\(varTy, varName) -> name == varName) (vars env) of
    Nothing -> failWith $ "Variable '" ++ name ++ "' not previously defined."
    Just (varTy, _) -> return varTy
