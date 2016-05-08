{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module FastoParser where

import Fasto
import Control.Applicative ((<*>),
                            (<*),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import qualified Data.Text as Text
import Data.Char
import Control.Monad

keywords :: [] String
keywords = ["if", "let", "in", "true", "false", "int", "char", "bool", "write", "read"]

parseProg :: String -> Either String Prog
parseProg s = runParserOn prog s

runParserOn :: Parser a -> String -> Either String a
runParserOn pars s =
  case parse pars $ Text.pack s of
  Done "" prg  -> return prg
  Done remd _ -> Left ("Parsing did not consume: " ++ show(remd))
  Fail _ _ _ -> Left ("Parsing failed.")
  Partial b       -> case feed (Partial b) (Text.pack "") of
    Done "" prg -> return prg
    Fail _ _ _ -> Left ("Parsing failed.")

prog :: Parser Prog
prog = many1 fun <* skipSpace

fun :: Parser FunDec
fun = do
  symbol "fun"
  tp <- tpRead
  id <- getName
  validID id
  symbol "("
  params <- params
  symbol ")"
  symbol "="
  e <- expr
  return $ Fun tp id params e

getName :: Parser String
getName = do
  skipSpace
  name_1 <- satisfy isAlpha
  name_n <- many' $ (satisfy (\c -> isAlpha c || isNumber c))
  return $ name_1 : name_n

tpRead :: Parser Type
tpRead =
  choice [ symbol "int" *> return Int,
           symbol "char" *> return Char,
           symbol "bool" *> return Bool ]

param :: Parser Param
param = do
  tp <- tpRead
  id <- getName
  return (tp, id)

params :: Parser ([] Param)
params = param `sepBy` symbol "," -- why not ","?

expr :: Parser Exp
expr = letExp

letExp :: Parser Exp
letExp = letBranch <|> ifExp

letExpSingle :: Parser (ID, Exp)
letExpSingle = do
  id <- getName
  symbol "="
  e <- ifExp
  return (id, e)

letBranch :: Parser Exp
letBranch = do
  symbol "let"
  decls <- many1 letExpSingle
  symbol "in"
  e <- letExp
  return $ LetExp decls e

ifExp :: Parser Exp
ifExp = ifBranch <|> orExp

ifBranch :: Parser Exp
ifBranch = do
  symbol "if"
  e0 <- orExp
  symbol "then"
  e1 <- letExp
  symbol "else"
  e2 <- letExp
  return $ IfExp e0 e1 e2

orExp :: Parser Exp
orExp = do
  tv <- andExp
  v <- orExpOpt tv
  return v

orExpOpt :: Exp -> Parser Exp
orExpOpt inval = orBranch inval
                  <|> return inval

orBranch :: Exp -> Parser Exp
orBranch inval = do
  symbol "||"
  tv <- andExp
  v <- orExpOpt (OrExp inval tv)
  return v

andExp :: Parser Exp
andExp = do
  tv <- eqExp
  v <- andExpOpt tv
  return v

andExpOpt :: Exp -> Parser Exp
andExpOpt inval = andBranch inval
                   <|> return inval

andBranch :: Exp -> Parser Exp
andBranch inval = do
  symbol "&&"
  tv <- eqExp
  v <- andExpOpt (AndExp inval tv)
  return v

eqExp :: Parser Exp
eqExp = eqBranch <|> neqBranch <|> ltExp

-- Equality is non-associative in this language
-- Left factorise if possible
eqBranch :: Parser Exp
eqBranch = do
  e0 <- ltExp
  symbol "=="
  e1 <- ltExp
  return $ EqExp e0 e1

neqBranch :: Parser Exp
neqBranch = do
  e0 <- ltExp
  symbol "!="
  e1 <- ltExp
  return $ NotExp (EqExp e0 e1)

ltExp :: Parser Exp
ltExp = ltBranch <|> plusExp

-- Left factorise if possible
ltBranch :: Parser Exp
ltBranch = do
  e0 <- plusExp
  symbol "<"
  e1 <- plusExp
  return $ LtExp e0 e1

plusExp :: Parser Exp
plusExp = do
  tv <- mulExp
  v <- plusExpOpt tv
  return v

plusExpOpt :: Exp -> Parser Exp
plusExpOpt inval = do
  plusBranch inval <|> minusBranch inval
  <|> return inval

plusBranch :: Exp -> Parser Exp
plusBranch inval = do
  symbol "+"
  tv <- mulExp
  v <- plusExpOpt (PlusExp inval tv)
  return $ v

minusBranch :: Exp -> Parser Exp
minusBranch inval = do
  symbol "-"
  tv <- mulExp
  v <- plusExpOpt (MinusExp inval tv)
  return $ v

mulExp :: Parser Exp
mulExp = do
  tv <- negateExp
  v <- mulExpOpt tv
  return v

mulExpOpt :: Exp -> Parser Exp
mulExpOpt inval = divBranch inval <|> mulBranch inval <|> modBranch inval
                   <|> return inval
                   
divBranch :: Exp -> Parser Exp
divBranch inval = do
  symbol "/"
  tv <- negateExp
  v <- mulExpOpt (DivExp inval tv)
  return $ v

mulBranch :: Exp -> Parser Exp
mulBranch inval = do
  symbol "*"
  tv <- negateExp
  v <- mulExpOpt (MultExp inval tv)
  return $ v

modBranch :: Exp -> Parser Exp
modBranch inval = do
  symbol "%"
  tv <- negateExp
  v <- mulExpOpt (ModExp inval tv)
  return $ v

negateExp :: Parser Exp
negateExp = negateBranch <|> notExp

negateBranch :: Parser Exp
negateBranch = do
  symbol "~"
  e0 <- negateExp
  return $ NegateExp e0

notExp :: Parser Exp
notExp = notBranch <|> bracketsExp

notBranch :: Parser Exp
notBranch = do
  symbol "!"
  e0 <- bracketsExp
  return $ NotExp e0

bracketsExp :: Parser Exp
bracketsExp = leaf <|> brackets expr

brackets :: Parser a -> Parser a
brackets p = do
  symbol "("
  r <- p
  symbol ")"
  return r

leaf :: Parser Exp
leaf = funCallLeaf <|> numLeaf <|> boolLeaf <|> charLeaf <|> idLeaf

numLeaf :: Parser Exp
numLeaf = do
  skipSpace
  num <- many1 (choice (map char ['0'..'9']))
  return $ Literal $ IntVal (read num :: Int)

boolLeaf :: Parser Exp
boolLeaf = do
  choice [ symbol "true" *> return (Literal (BoolVal True)),
           symbol "false" *> return (Literal (BoolVal False))]

charLeaf :: Parser Exp
charLeaf = do
  skipSpace
  string "'"
  ch <- choice (map char (['a'..'z'] ++ ['A'..'Z']))
  string "'"
  return $ Literal $ CharVal ch

idLeaf :: Parser Exp
idLeaf = do
  id <- getName
  validID id
  return $ ID id

funCallLeaf :: Parser Exp
funCallLeaf = do
  id <- getName
  symbol "("
  exps <- expr `sepBy` symbol ","
  symbol ")"
  return $ FunCall id exps

symbol :: Text.Text -> Parser Text.Text
symbol s = skipSpace *> (string s)

validID :: String -> Parser ()
validID s =
  when (s `elem` keywords) $ fail "Keywords may not be used as identifiers."

