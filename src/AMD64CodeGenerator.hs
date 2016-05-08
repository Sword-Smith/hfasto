{-# LANGUAGE OverloadedStrings #-}
module AMD64CodeGenerator where

import Imp as I
-- For testing START
import Data.Either
import FastoCodeGenerator as FCG hiding (compileExp, compileFun)
import FastoParser as FP
import Control.Monad.State.Lazy
import RegAlloc
-- For testing END
import AMD64

-- ncsr is Number of Caller-Save (volatile) Registers
compile :: Int -> [] RealReg -> I.Prog -> String
compile ncsr rp prg = ppAMD64Prog rp (compileProg ncsr rp prg)

-- Use putStrLn to print it nicely
ppAMD64Prog :: [] RealReg -> [] AMD64.Instruction -> String
ppAMD64Prog rp insts = concat (map (++ "\n") (map ppAMD64 insts))

compileProg :: Int -> [] RealReg -> I.Prog -> [] AMD64.Instruction
compileProg ncsr rp prg = concat $ map (compileFun ncsr rp) prg

-- Write functions for testing that compile without reg alloc
compileFun :: Int -> [] RealReg -> I.Function -> [] AMD64.Instruction
compileFun ncsr rp (Fun (FunHead fid args) insts) =
  let
    function =
      MipsFunction { funPrologue = prologue,
                     funBody = loadArgRegs args ++ concat (map compileExp insts),
                     funEpilogue = epilogue
                   }
  in
    AMD64.Label fid : (regAllocInsts ncsr rp function)

prologue :: [] AMD64.Instruction
prologue = [AMD64.PUSH "rbp",
            AMD64.MOV "rbp" "rsp"]

epilogue :: [] AMD64.Instruction
epilogue = [AMD64.POP "rbp",
            AMD64.RET]

saveArgRegs :: [] String -> [] AMD64.Instruction
saveArgRegs args = saveArgRegsH 0 args

saveArgRegsH :: Int -> [] String -> [] AMD64.Instruction
saveArgRegsH _ _ = []

loadArgRegs :: [] String -> [] AMD64.Instruction
loadArgRegs args = saveArgRegsH 0 args

loadArgRegsH :: Int -> [] String -> [] AMD64.Instruction
loadArgRegsH _ _ = []

loadRegisters :: [] String -> [] AMD64.Instruction
loadRegisters _ = []

compileExp :: I.Instruction -> [] AMD64.Instruction
compileExp (ADeclInst rd (ID rt))                = [AMD64.MOV rd rt]
compileExp (ADeclInst rd (I.IntVal imm))         = [AMD64.MOV rd (show imm)]
compileExp (BDeclInst rd rs Plus (ID rt))        = [AMD64.MOV "rax" rs,
                                                   AMD64.ADD "rax" rt,
                                                   AMD64.MOV rd "rax"]
compileExp (BDeclInst rd rs Minus (ID rt))       = [AMD64.MOV "rax" rs,
                                                   AMD64.SUB "rax" rt,
                                                   AMD64.MOV rd "rax"]
compileExp (BDeclInst rd rs Minus (ID rt))       = [AMD64.MOV "rax" rs,
                                                   AMD64.SUB "rax" rt,
                                                   AMD64.MOV rd "rax"]
compileExp (BDeclInst rd rs Minus (IntVal imm))  = [AMD64.MOV "rax" rs,
                                                   AMD64.SUB "rax" (show imm),
                                                   AMD64.MOV rd "rax"]
compileExp (BDeclInst rd rs Mult (ID rt))        = [AMD64.MOV "rax" rs,
                                                   AMD64.MUL rt,
                                                   AMD64.MOV rd "rax"]
compileExp (BDeclInst rd rs Div (ID rt))         = [AMD64.MOV "rax" rs,
                                                   AMD64.DIV rt,
                                                   AMD64.MOV rd "rax"]
compileExp (FDeclInst target funName args)       = saveArgRegs args ++ [AMD64.CALL funName] ++ [AMD64.MOV target "rax"]
compileExp (ReturnInst retName)                  = [AMD64.MOV "rax" retName] -- the rest happens in function epilogue. 
compileExp (IfInst v1 Eq (I.IntVal i ) eqL neqL) = [AMD64.CMP v1 (show i),
                                                    AMD64.JE eqL,
                                                    AMD64.JMP neqL]
compileExp (IfInst v1 Eq (I.ID v2 ) eqL neqL)    = [AMD64.CMP v1 v2,
                                                    AMD64.JE eqL,
                                                    AMD64.JMP neqL]
compileExp (I.GotoInst l)                        = [AMD64.JMP l]
compileExp (I.Label l)                           = [AMD64.Label l]

ppAMD64 :: AMD64.Instruction -> String
ppAMD64 (AMD64.Label l)   = l ++ ":"
ppAMD64 (AMD64.Comment s) = ";;" ++ s
ppAMD64 (AMD64.CMP r0 r1) = "cmp " ++ r0 ++ ", " ++ r1;
ppAMD64 (AMD64.JMP l)     = "jmp " ++ l
ppAMD64 (AMD64.JNE l)     = "jne " ++ l
ppAMD64 (AMD64.JZ l)      = "jz " ++ l
ppAMD64 (AMD64.JE l)      = "je " ++ l
ppAMD64 (AMD64.INC reg)   = "inc " ++ reg
ppAMD64 (AMD64.DEC reg)   = "dec " ++ reg
ppAMD64 (AMD64.ADD r0 r1) = "add " ++ r0 ++ ", " ++ r1
ppAMD64 (AMD64.IMUL reg)  = "imul " ++ reg
ppAMD64 (AMD64.MUL reg)   = "mul " ++ reg
ppAMD64 (AMD64.IDIV reg)  = "idiv " ++ reg
ppAMD64 (AMD64.DIV reg)   = "div " ++ reg
ppAMD64 (AMD64.SUB r0 r1) = "sub " ++ r0 ++ ", " ++ r1
ppAMD64 (AMD64.INT hex)   = "int " ++ hex
ppAMD64 (AMD64.PUSH reg)  = "push " ++ reg
ppAMD64 (AMD64.POP reg)   = "pop " ++ reg
ppAMD64 (AMD64.RET)       = "ret"
ppAMD64 (AMD64.CALL l)    = "call " ++ l


testAMD6410 =
  AMD64CodeGenerator.compile 0 [] (evalState (FCG.compile (head (rights [(FP.parseProg "fun int main(int a) = if (a == 0) then 1 else if (a == 1) then 1 else main(a-1) + main(a-2)")]))) (CodeEnv 0))
