module MipsRegAllocTest where

import MipsRegAlloc
import MipsCodeGenerator as MCG
import Control.Monad.State.Lazy
import Mips
import Imp
import qualified Data.Set as S

filterDynamicRegsTest :: String -> Bool -> Bool
filterDynamicRegsTest reg expec = (filterDynamicRegs reg) == expec

test0 :: Bool
test0 =
  let
    testl0 = filterDynamicRegsTest "var1" True
    testl1 = filterDynamicRegsTest "$t0" False
    testls = [testl0, testl1]
  in
    foldl (&&) True testls

-- Simple liveness test
test1 :: Bool
test1 =
  let
    instsTest0 = [ Mips.LW  "var0" "$sp" "4",
                   Mips.LW  "var1" "$sp" "0",
                   Mips.ADD "var2" "var1" "var0",
                   Mips.ADD "var2" "var1" "var2",
                   Mips.XOR "$v0" "var2" "var1",
                   Mips.JR "$ra"
                 ]
    (livInSets0, livOutSets0, killSets0, generatedSets0) = getLiveness instsTest0
    testl0 = livOutSets0 == [S.fromList ["var0"],
                            S.fromList ["var0","var1"],
                            S.fromList ["var1","var2"],
                            S.fromList ["var1","var2"],
                            S.fromList [],
                            S.fromList []]
    testl1 = livInSets0 == [S.fromList [],
                           S.fromList ["var0"],
                           S.fromList ["var0","var1"],
                           S.fromList ["var1","var2"],
                           S.fromList ["var1","var2"],
                           S.fromList []]
    testl2 = killSets0 == [S.fromList ["var0"],
                          S.fromList ["var1"],
                          S.fromList ["var2"],
                          S.fromList ["var2"],
                          S.fromList [],
                          S.fromList []]
    testl3 = generatedSets0 == [S.fromList [],
                               S.fromList [],
                               S.fromList ["var0","var1"],
                               S.fromList ["var1","var2"],
                               S.fromList ["var1","var2"],
                               S.fromList []]
    testls = [testl0, testl1, testl2, testl3]
  in
    foldl (&&) True testls

-- funPL :: [] Mips.Instruction
-- funPL = [ Mips.SUBI "$sp" "$sp" "-8",
--           Mips.SW "$ra" "$sp" "4",
--           Mips.SW "$fp" "$sp" "0",
--           Mips.XOR "$fp" "$sp" "0"]
-- Something that generates the liveness analysis for IML function call
--test2 :: Bool
test2 =
  let
    fdecl        = Imp.FDeclInst "target" "funName" $ map ( "arg" ++ ) (map show [0..8])
    compiledMips = (MCG.compileExp fdecl ) ++ [Mips.JR "$ra"]
  in
    compiledMips

test4 =
  let
    fdecl        = Imp.FDeclInst "target" "funName" (map show [0..8])
    compiledMips = MCG.compileExp fdecl ++ [Mips.JR "$ra"]
    (i, o, k, g) = getLiveness compiledMips    
  in
    (i, o, k, g)

test5 =
  let
    fdecl           = Imp.FDeclInst "target" "funName" (map show [0..8])
    compiledMips    = MCG.compileExp fdecl ++ [Mips.JR "$ra"]
    (i, o, k, g)    = getLiveness compiledMips
    interferenceSet = getInterferenceSet i k
  in
    interferenceSet

test6 =
  let
    fdecl           = Imp.FDeclInst "target" "funName" (map show [0..8])
    compiledMips    = MCG.compileExp fdecl ++ [Mips.JR "$ra"]
    (i, o, k, g)    = getLiveness compiledMips
    interferenceSet = getInterferenceSet i k
    sRegNames       = getSRegNames k g
  in
    sRegNames

test7 =
  let
    sRegNames       = test6
    intfSet         = test5
    vtable          = colorGraph (map ("$" ++) (map show [0..8])) (sRegNames, intfSet)
  in
    vtable
