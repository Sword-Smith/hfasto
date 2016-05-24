{-# LANGUAGE TypeSynonymInstances #-}
module FastoCodeGenerator where

import Fasto as F
import FastoParser as FP -- used for testing only
import Imp as I

import Data.Char
import Data.Either -- used for testing only
import Data.Maybe -- This ought not be necessary. Used poorly.
import Control.Monad.State.Lazy

type Vtable = [] (F.ID, I.ValID)
type Ftable = [] (F.ID, I.FunID)

data CodeEnv = CodeEnv { varCount :: Int
                       }
             deriving Show

type CodeGen a = State CodeEnv a

-- Constants
readFunction retName argTuple  = [FDeclInst retName "readIntegerFunction" (map fst argTuple)]
writeFunction retName argTuple = [FDeclInst retName "writeIntegerFunction" (map fst argTuple)]

-- Fix me. Utilize Maybe monad better.
-- Should an invalid lookup stop the monadic calculation? How does State handle this?
findID :: F.ID -> Vtable -> CodeGen (Maybe I.ValID)
findID fastoID ((fid, iid):xs) = do
  case (fid == fastoID) of True -> return $ Just iid
                           False -> findID fastoID xs
findID _ _ = do
  return Nothing

-- This allows multiple bindings of the same name. Should that be prevented?
bindID :: (F.ID, I.ValID) -> Vtable -> CodeGen Vtable
bindID (fid, iid) vtable = do
  return $ (fid, iid) : vtable

-- This could probably be written in a prettier way
bindIDs :: [] (F.ID, I.ValID) -> Vtable -> CodeGen Vtable
bindIDs ((fid, iid):ls) vtable = bindID (fid, iid) vtable >>= \x -> bindIDs ls x
bindIDs _ vtable = do
  return vtable

emptyVtable :: Vtable
emptyVtable = []

-- Function table declarations
findFID :: F.ID -> Ftable -> CodeGen (Maybe I.FunID)
findFID = findID

bindFID :: (F.ID, I.ValID) -> Ftable -> CodeGen Ftable
bindFID = bindID

emptyFtable :: Ftable
emptyFtable = []

setCode :: [] I.Instruction -> CodeGen ([] I.Instruction)
setCode = return

incr :: CodeGen Int
incr = do
  codeEnv <- get
  let i = varCount codeEnv
  put codeEnv { varCount = i + 1 }
  return i

newLabel, newVar :: String -> CodeGen String
newLabel labelName = do
  i <- incr
  return $ labelName ++ show i

newVar = newLabel

bindFName :: Ftable -> F.FunDec -> CodeGen Ftable
bindFName ftable f = do
  fname <- newLabel "fun"
  ftable' <- bindFID (funName f, fname) ftable
  return ftable'

bindFNames :: F.Prog -> CodeGen Ftable
bindFNames functions = foldM bindFName [] functions

compile :: F.Prog -> CodeGen I.Prog
compile program = do
  ftable <- bindFNames program
  progr <- compileH program ftable
  return $ progr

compileH :: F.Prog -> Ftable -> CodeGen I.Prog
compileH functions ftable = mapM (compileFun ftable) functions

-- compileExps :: [] I.ValID -> Vtable -> Ftable -> F.Exp -> CodeGen ([] I.Instruction)
-- compileExps vnames vtable ftable exps = mapM (compileExp )

-- Some work is required to look at how the arguments should be passed and how
-- the vtable should be set.
-- The call convention should imo be handled at the next stage of the compiler
-- such that the call convention can be made architecture-dependent as it should
-- be. That would mean that the 1st stage of compiling a function (in the next
-- stage) would be to load to the variable with which the function is called
-- into the symbolical register names found in the function header below
compileFun :: Ftable -> F.FunDec -> CodeGen I.Function
compileFun ftable F.Fun { funType = ftp,
                          funName = fname,
                          funParams = fparams,
                          funBody = fExp } = do
  ownName <- findFID fname ftable
  vName <- case ownName of
    Just name -> newVar $ "ret_" ++ name ++ "_"
    _         -> error $ "Invalid function name."
  argNames <- mapM newVar (replicate (length fparams) "var_")
  vtableInit <- bindIDs (zip (map snd fparams) argNames) emptyVtable -- This is fairly ugly also
  -- perhaps the string of vName should not be passed to the compileExp call since this might manipulate vName.
  iInsts <- compileExp vName vtableInit ftable fExp
  return $ I.Fun (FunHead (fname) argNames) (iInsts ++ [ ReturnInst vName ]) -- ineffective?

compileExp :: I.ValID -> Vtable -> Ftable -> F.Exp -> CodeGen ([] I.Instruction)
-- How should error in the monad be handled?
compileExp retName vtable _ (F.ID varName) = do
  iid <- findID varName vtable
  case iid of
    Just id -> do return [ADeclInst retName (I.ID id)]
    Nothing -> error $ "Function returned " ++ varName ++ " which is undeclared."
compileExp retName _  _ (Literal (F.IntVal int)) = do
  return $ [ADeclInst retName (I.IntVal int)]
compileExp retName _ _ (Literal (F.BoolVal True)) = do
  return $ [ADeclInst retName (I.IntVal 1)]
compileExp retName _ _ (Literal (F.BoolVal False)) = do
  return $ [ADeclInst retName (I.IntVal 0)]
compileExp retName _ _ (Literal (F.CharVal char)) = do
  return $ [ADeclInst retName (I.IntVal $ ord char)]
compileExp retName vtable ftable (NegateExp exp1) = do
  v1Name <- newVar "negate_"
  e1 <- compileExp v1Name vtable ftable exp1
  return $ e1 ++ [UDeclInst retName Negate (I.ID v1Name)]
compileExp retName vtable ftable (PlusExp exp1 exp2) = do
  v1Name <- newVar "plus1_"
  v2Name <- newVar "plus2_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  return $ e1 ++ e2 ++ [BDeclInst retName v1Name Plus (I.ID v2Name)]
compileExp retName vtable ftable (MinusExp exp1 exp2) = do
  v1Name <- newVar "minus1_"
  v2Name <- newVar "minus_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  return $ e1 ++ e2 ++ [BDeclInst retName v1Name Minus (I.ID v2Name)]
compileExp retName vtable ftable (MultExp exp1 exp2) = do
  v1Name <- newVar "mult1_"
  v2Name <- newVar "mult2_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  return $ e1 ++ e2 ++ [BDeclInst retName v1Name Mult (I.ID v2Name)]
compileExp retName vtable ftable (DivExp exp1 exp2) = do
  v1Name <- newVar "div1_"
  v2Name <- newVar "div2_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  return $ e1 ++ e2 ++ [BDeclInst retName v1Name Div (I.ID v2Name)]
compileExp retName vtable ftable (ModExp exp1 exp2) = do
  v1Name <- newVar "mod1_"
  v2Name <- newVar "mod2_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  return $ e1 ++ e2 ++ [BDeclInst retName v1Name Mod (I.ID v2Name)]
compileExp retName vtable ftable (EqExp exp1 exp2) = do
  v1Name <- newVar "eq1_"
  v2Name <- newVar "eq2_"
  e1 <- compileExp v1Name vtable ftable exp1
  e2 <- compileExp v2Name vtable ftable exp2
  eqL <- newLabel "eq_"
  neqL <- newLabel "neq_"
  code1 <- setCode [ADeclInst retName (I.IntVal 0)]
  code2 <- setCode [IfInst v1Name Eq (I.ID v2Name) eqL neqL,
                    Label eqL,
                    ADeclInst retName (I.IntVal 1),
                    Label neqL]
  return $ e1 ++ e2 ++ code1 ++ code2
compileExp retName vtable ftable (AndExp exp1 exp2) = do
  v1Name <- newVar "and"
  code1 <- setCode [ADeclInst retName (I.IntVal 0)]
  e1 <- compileExp v1Name vtable ftable exp1
  trueL <- newLabel "firsttrue"
  falseL <- newLabel "firstfalse"
  code2 <- setCode [IfInst v1Name Eq (I.IntVal 1) trueL falseL,
                    Label trueL] -- Could also be neq 0 (ought?)
  e2 <- compileExp retName vtable ftable exp2
  code3 <- setCode [Label falseL]
  return $ code1 ++ e1 ++ code2 ++ e2 ++ code3
compileExp retName vtable ftable (OrExp exp1 exp2) = do
  v1Name <- newVar "or"
  code1 <- setCode [ADeclInst retName (I.IntVal 1)]
  e1 <- compileExp v1Name vtable ftable exp1
  trueL <- newLabel "firsttrue"
  falseL <- newLabel "firstfalse"
  code2 <- setCode [IfInst v1Name Eq (I.IntVal 1) trueL falseL,
                    Label falseL] -- Could also be neq 0
  e2 <- compileExp retName vtable ftable exp2
  code3 <- setCode [Label trueL]
  return $ code1 ++ e1 ++ code2 ++ e2 ++ code3
compileExp retName vtable ftable (LetExp decls exp) = do
  v1 <- newVar "let_binding_result"
  --v2 <- newVar "let_in_result"
  (vtable', letinsts) <- bindToVtable decls vtable ftable []
  e <- compileExp retName vtable' ftable exp
  return $ letinsts ++ e
compileExp retName vtable ftable (IfExp exp1 exp2 exp3) = do
  v1Name <- newVar "if_header"
  trueL <- newLabel "if_true"
  falseL <- newLabel "if_false"
  endL <- newLabel "if_end"
  e1 <- compileExp v1Name vtable ftable exp1
  code1 <- setCode [IfInst v1Name Eq (I.IntVal 1) trueL falseL, Label trueL] -- Could also be neq 0
  e2 <- compileExp retName vtable ftable exp2
  code2 <- setCode [I.GotoInst endL, Label falseL]
  e3 <- compileExp retName vtable ftable exp3
  code3 <- setCode [Label endL]
  return $ e1 ++ code1 ++ e2 ++ code2 ++ e3 ++ code3
compileExp retName vtable ftable (FunCall fid args) = do -- How should a list of arguments be compiled?
  argTuple <- handleArgs vtable ftable args
  callFunName <- findFID fid ftable
  case callFunName of
    Just name -> do
      funCallCode <- setCode [FDeclInst retName fid (map fst argTuple)]
      return $ (concat (map snd argTuple)) ++ funCallCode
    Nothing -> do
      -- Insert a lookup of imported library functions here
      if (fid == "read")
      then do
        funCallCode <- setCode (readFunction retName argTuple)
        return $ (concat (map snd argTuple)) ++ funCallCode
      else
        if (fid == "write")
        then do
          funCallCode <- setCode (writeFunction retName argTuple)
          return $ (concat (map snd argTuple)) ++ funCallCode
        else
          error $ "Undeclared function called: " ++ fid

-- What about bindings to ftable inside let expression?
bindToVtable ((id,exp):decls) vtable ftable insts = do
  v <- newVar "let_binding_result"
  e <- compileExp v vtable ftable exp
  ret <- bindToVtable decls ((id, v):vtable) ftable (insts ++ e)
  return $ ret
bindToVtable [] vtable _ insts = return (vtable, insts)

handleArgs :: Vtable -> Ftable -> [] Exp -> CodeGen [(ValID, [] Instruction)]
handleArgs vtable ftable exps = mapM (handleArg vtable ftable) exps

handleArg :: Vtable -> Ftable -> Exp -> CodeGen (ValID, [] Instruction)
handleArg vtable ftable e = do
  retName <- newVar "funarg_"
  e0 <- compileExp retName vtable ftable e
  return ( retName, e0 )

