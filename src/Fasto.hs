module Fasto where

-- type er aliaser
-- data er nye typer. Alle declaration skal have værdikonstruktører.

type Prog = [FunDec]

data FunDec = Fun { funType :: Type
                  , funName :: ID
                  , funParams :: [Param]
                  , funBody :: Exp
                  }
            deriving Show

type Param = (Type, ID)

data Type = Int
          | Char
          | Bool
          deriving (Show, Eq)

type ID = String

data Exp = ID ID
         | Literal Literal
         | PlusExp Exp Exp
         | MinusExp Exp Exp
         | MultExp Exp Exp
         | DivExp Exp Exp
         | ModExp Exp Exp
         | EqExp Exp Exp
         | LtExp Exp Exp
         | AndExp Exp Exp
         | OrExp Exp Exp
         | NegateExp Exp
         | NotExp Exp
         | IfExp Exp Exp Exp
         | LetExp [(ID, Exp)] Exp
         | FunCall ID ([] Exp) deriving Show

data Literal = IntVal Int
             | CharVal Char
             | BoolVal Bool deriving Show
