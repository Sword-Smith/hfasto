module Imp where

type Label = String
type FunID = String
type ValID = String
type Arg   = ValID

type Prog = [] Function

data Function = Fun Header ([] Instruction ) deriving (Show, Eq)

data Header = FunHead FunID ([] Arg) deriving (Show, Eq)

data Instruction = Label Label
                 | ADeclInst ValID Atom
                 | UDeclInst ValID Unop Atom
                 | BDeclInst ValID ValID Binop Atom
                 | FDeclInst ValID FunID ([] Arg)
                 | MRInst ValID Atom
                 | MWInst Atom ValID
                 | GotoInst Label
                 | IfInst ValID Relop Atom Label Label -- perhaps only ONE label is needed here?
                 | ReturnInst ValID deriving (Show, Eq)

-- Note that the IML only has int types. Maybe bools and chars should be added?
-- I don't know if that would be a good idea.
-- If type check is done before this step, then it should be unnecessary.
-- Eventually floats should be added also.
data Atom = ID ValID
          | IntVal Int deriving (Show, Eq)

data Unop = Not
          | Negate deriving (Show, Eq)

-- Bitwise operators could be added here also.
data Binop = Plus
           | Minus
           | Mult
           | Div
           | Mod deriving (Show, Eq)

-- Add =< and => here also
data Relop = Lt
           | Gt
           | Eq
           | Neq deriving (Show, Eq)
