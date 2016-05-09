module MipsCodeGenerator where

import Data.Char 
import Data.Either
import Imp as I
-- For testing START
import FastoCodeGenerator as FCG hiding (compileExp, compileFun)
import FastoParser as FP
import Control.Monad.State.Lazy
import MipsRegAlloc
-- For testing END
import Mips
import MipsIO

zero :: String
zero = "$0"

-- ncsr is Number of Caller-Save Registers
compile :: Int -> [] RealReg -> I.Prog -> String
compile ncsr rp prg = ppMipsProg rp (compileProg ncsr rp prg)

compileNoPP :: I.Prog -> [] Mips.Instruction
compileNoPP prg = compileProgNoPP prg

compileNoRA :: I.Prog -> String
compileNoRA prg = compileProgNoRA prg

-- Use putStrLn to print it nicely
ppMipsProg :: [] RealReg -> [] Mips.Instruction -> String
ppMipsProg rp insts = concat (map (++ "\n") (map ppMips insts))

programInit :: [] Mips.Instruction
programInit = [Mips.DeclGlobal "_start", Mips.DotText, Mips.Label "_start", Mips.JAL "main"]

programEnd :: [] Mips.Instruction
programEnd = [Mips.XOR "$a0" "$v0" zero, Mips.XORI "$v0" zero "17", Mips.Syscall]

-- programStart :: [] Mips.Instruction
-- programStart = [ Mips.Label "main" ]

compileProg :: Int -> [] RealReg -> I.Prog -> [] Mips.Instruction
compileProg ncsr rp prg = programInit ++ programEnd ++ MipsIO.readIntegerFunction ++ MipsIO.writeIntegerFunction ++ (concat $ map (compileFun ncsr rp) prg)

compileProgNoPP :: I.Prog -> [] Mips.Instruction
compileProgNoPP prg = concat $ map compileFunNoPP prg

compileProgNoRA :: I.Prog -> String
compileProgNoRA prg =  ppMipsProgNoRegAlloc $ concatMap compileFunNoRA prg 

compileFunNoRA :: I.Function -> [] Mips.Instruction
compileFunNoRA (Fun (FunHead fid args) instructions) =
      [Mips.Label fid] ++
      prologue ++
      loadRegisters args ++ concat (map compileExp (instructions)) ++
      epilogue

compileFunNoPP :: I.Function -> [] Mips.Instruction
compileFunNoPP (Fun (FunHead fid args) instructions) =
  [Mips.Label fid] ++
  prologue ++
  loadRegisters args ++ concat (map compileExp (instructions)) ++
  epilogue

compileFun :: Int -> [] RealReg -> I.Function -> [] Mips.Instruction
compileFun ncsr rp (Fun (FunHead fid args) insts) =
  let
    function =
      MipsFunction { funPrologue = prologue,
                     funBody = loadRegisters args ++ concat (map compileExp insts),
                     funEpilogue = epilogue
                   }
  in
    [Mips.Label fid] ++ (regAllocInsts ncsr rp function)

-- Storing the $RA is not needed for leaf functions! Can prob be removed in optimizations.
prologue :: [] Mips.Instruction
prologue = [Mips.ADDI "$sp" "$sp" "-8",
            Mips.SW "$ra" "$sp" "4",
            Mips.SW "$fp" "$sp" "0",
            Mips.XOR "$fp" "$sp" "$0"]

epilogue :: [] Mips.Instruction
epilogue = [Mips.LW "$fp" "$sp" "0",
            Mips.LW "$ra" "$sp" "4",
            Mips.ADDI "$sp" "$sp" "8",
            JR "$ra"]

saveArgRegs :: [] String -> [] Mips.Instruction
saveArgRegs args = saveArgRegsH 0 args

saveArgRegsH :: Int -> [] String -> [] Mips.Instruction
saveArgRegsH _ []         = []
saveArgRegsH i (arg:args) = if i < 4
                            then Mips.XOR ("$a" ++ show i) arg zero :
                                 Mips.SW arg "$sp" (show (i*4)) :
                                 saveArgRegsH (i+1) args
                            else Mips.SW arg "$sp" (show (i*4)) :
                                 saveArgRegsH (i+1) args

-- Atm, no proper liveness analysis is done with these "string:strings"
loadRegisters :: [] String -> [] Mips.Instruction
loadRegisters args = loadRegistersH 0 args

-- The "8" in the below function cannot be hardcoded since the register allocator
-- may invalidate that number.
-- The solution may be to introduce a loadRegisters value with which the
-- register allocator may shift all the special, symbolic lw operations?
-- They can also just be rewritten if the are part of the loadArgs part of the
-- MipsFunction struct.
loadRegistersH :: Int -> [] String -> [] Mips.Instruction
loadRegistersH _ []         = []
loadRegistersH i (arg:args) = if i < 4
                              then Mips.XOR arg ("$a" ++ (show i)) zero :
                                   loadRegistersH (i + 1) args
                              else Mips.LW arg "$sp" (show (i*4 + 8)) :
                                   loadRegistersH (i + 1) args

-- loadRegistersH :: [] String -> [] Mips.Instruction
-- loadRegistersH args =
--   let loadRegistersH (string:strings) i =
--         if (i < 4)
--         then (Mips.XOR string ("$a" ++ [intToDigit i]) zero) : loadRegistersH strings (i+1)
--         else [] -- Here, the registers should be read from mem if compiler is to handle function calls with more than 4 args
--       loadRegistersH (_) _ = []
--   in
--     loadRegistersH args 0

compileExp :: I.Instruction -> [] Mips.Instruction
compileExp (ADeclInst rd (ID rt))                = [Mips.XOR rd rt zero]
compileExp (ADeclInst rd (I.IntVal imm))         = [Mips.XORI rd zero (show imm)]
compileExp (UDeclInst rd Negate (ID rt))         = [Mips.SUB rd zero rt]
compileExp (UDeclInst rd Negate (I.IntVal imm))  = [Mips.SUBI rd zero (show imm)]
compileExp (BDeclInst rd rs Plus (ID rt))        = [Mips.ADD rd rs rt]
compileExp (BDeclInst rd rs Minus (ID rt))       = [Mips.SUB rd rs rt]
compileExp (BDeclInst rd rs Minus (IntVal imm))  = [Mips.SUBI rd rs (show imm)]
compileExp (BDeclInst rd rs Mult (ID rt))        = [Mips.MUL rd rs rt]
compileExp (BDeclInst rd rs Div (ID rt))         = [Mips.DIV rd rs rt]
compileExp (FDeclInst target funName args)       =
  let
    n = length args
  in
    Mips.SUBI "$sp" "$sp" (show (4*n)) :
    saveArgRegs args ++
    [Mips.JAL funName] ++
    [Mips.XOR target "$v0" zero] ++
    [Mips.ADDI "$sp" "$sp" (show (4*n))]
compileExp (ReturnInst retName)             = [Mips.XOR "$v0" retName zero] -- rest happens in function epilogue
compileExp (IfInst v1 Eq (I.IntVal i ) eqL neqL) = [Mips.XORI "compReg" zero (show i),
                                                    Mips.BNE v1 "compReg" neqL,
                                                    Mips.J eqL]
compileExp (IfInst v1 Eq (I.ID v2 ) eqL neqL) = [Mips.BNE v1 v2 neqL,
                                                 Mips.J eqL]
compileExp (I.GotoInst l)                   = [Mips.J l]
compileExp (I.Label l)                      = [Mips.Label l]

ppMipsProgNoRegAlloc :: [] Mips.Instruction -> String
ppMipsProgNoRegAlloc instructions = concat (map (++ "\n") (map ppMips instructions))

ppMips :: Mips.Instruction -> String
ppMips inst = case inst of
  Mips.Label l        -> l ++ ":"
  Mips.Comment s      -> "# " ++ s
  Mips.XOR rd rs rt   -> "XOR " ++ rd ++ ", " ++ rs ++ ", " ++  rt
  Mips.XORI rd rs imm -> "XORI " ++ rd ++ ", " ++ rs ++ ", " ++ imm
  Mips.ADD rd rs rt   -> "ADD " ++ rd ++ ", " ++ rs ++ ", " ++ rt
  Mips.ADDI rd rs imm -> "ADDI " ++ rd ++ ", " ++ rs ++ ", " ++ imm
  Mips.SUB rd rs rt   -> "SUB " ++ rd ++ ", " ++ rs ++ ", " ++ rt
  Mips.SUBI rd rs imm -> "SUB " ++ rd ++ ", " ++ rs ++ ", " ++ imm
  Mips.MUL rd rs rt   -> "MUL " ++ rd ++ ", " ++ rs ++ ", " ++ rt
  Mips.DIV rd rs rt   -> "DIV " ++ rd ++ ", " ++ rs ++ ", " ++ rt
  Mips.SW rs rt imm   -> "SW " ++ rs ++ ", " ++ imm ++ "(" ++ rt ++ ")"
  Mips.LW rs rt imm   -> "LW " ++ rs ++ ", " ++ imm ++ "(" ++ rt ++ ")"
  Mips.JAL rd         -> "JAL " ++ rd
  Mips.JR rd          -> "JR " ++ rd
  Mips.J l            -> "J " ++ l
  Mips.BNE rs rt l    -> "BNE " ++ rs ++ ", " ++ rt ++ ", " ++ l
  Mips.Syscall        -> "syscall"
  -- Mips program headers below here
  Mips.DeclGlobal l   -> ".globl " ++ l
  Mips.DotText        -> ".text"

compileAFile :: String -> String
compileAFile sourceProg =
  MipsCodeGenerator.compile 8 (map (\x -> '$':(show x)) ([8..15])) (evalState (FCG.compile (head (rights [(FP.parseProg sourceProg)]))) (CodeEnv 0))
