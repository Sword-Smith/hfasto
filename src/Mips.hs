module Mips where

type Reg  = String
type Imm  = String
type Addr = String

-- Type and functions for the abstract syntax of MIPS

--data Instruction = RType :+: 
-- data Instruction = RType
--                  | IType
--                  | JType
--                  | Misc
                   
-- data RType = ADD Reg Reg Reg
--            | SUB Reg Reg Reg
--            | MUL Reg Reg Reg
--            | DIV Reg Reg Reg
--            | XOR Reg Reg Reg
--            | SLT Reg Reg Reg

-- data IType = SUBI Reg Reg Imm
--            | XORI Reg Reg Imm
--            | SLTI Reg Reg Imm
--            | BEQ Reg Reg Addr
--            | BNE Reg Reg Addr

data Instruction = Label String 
                 | Comment String
                 | LA Reg Addr
                 | LUI Reg Imm
                 | LB String String String
                 | LW String String Imm
                 | SW String String Imm
                 | SB String String String
                 | ADD Reg Reg Reg
                 | ADDI Reg Reg Imm
                 | SUB Reg Reg Reg
                 | SUBI Reg Reg Imm
                 | MUL Reg Reg Reg
                 | DIV Reg Reg Reg

                 | XOR Reg Reg Reg
                 | XORI Reg Reg Imm

                 | SLT Reg Reg Reg
                 | SLTI Reg Reg Imm
                 | BEQ Reg Reg Addr
                 | BNE Reg Reg Addr
                 | J Addr
                 | JR Reg -- Because of code in RegAlloc, may only be used for returning from function
                 | JAL Addr
                 | Nop
                 | Syscall
                 | Globl String
                 | Text String
                 | Data String
                 | Space String
                 | Ascii String
                 | Asciiz String
                 | Align String
                   -- Mips program headers below here
                 | DeclGlobal String
                 | DotText deriving (Show, Eq)
                   

