module MipsInterpreter where

import Control.Monad.State.Lazy
import Data.Word
import Data.Int
import Data.Bits

data MipsState = MipsState { intRegisters   :: [] Int32,
                             floatRegisters :: [] Double,
                             memory         :: [] Word8
                           } deriving (Show)

data MipsEnv a = State MipsState

initState :: MipsEnv ()
initState = do
  state <- get
  let iregs = intRegisters state
  put state { intRegisters = replicate 32 0, memory = replicate 640000 0 }

setReg :: Int -> Int32 -> MipsEnv ()
setReg tr tv = do
  state <- get
  let regs = intRegisters state
  put state { intRegisters = (take tr regs) ++ [tv] ++ (drop (tr+1) regs) }
  return ()

getReg :: Int -> MipsEnv Int32
getReg sr = do
  state <- get
  let regs = intRegisters state
  return (regs !! sr)

-- works as store word
-- MIPS is (according to Oleks big endian) 
setMem :: Int -> Int32 -> MipsEnv ()
setMem ta tv = do
  mipsEnv <- get
  let
    fi  = fromIntegral :: Int32 -> Word8
    mem = memory mipsEnv
    regsI = intRegisters mipsEnv
    regsF = floatRegisters mipsEnv
    r3  = fi $ tv `mod` (1 `shift` 8 )                 -- least significant
    r2  = fi $ (tv `shift` (-8)) `mod` (1 `shift` 8)
    r1  = fi $ (tv `shift` (-8*2)) `mod` (1 `shift` 8)
    r0  = fi $ (tv `shift` (-8*3)) `mod` (1 `shift` 8)
    in do
    put mipsEnv { intRegisters = regsI,
                floatRegisters = regsF,
                memory = ((take ta mem) ++ [r0,r1,r2,r3] ++ (drop (ta+4) mem)) }
