{-# LANGUAGE TemplateHaskell #-}
module AMD64RegAlloc where

import Control.Monad.State.Lazy
import Data.Char
import Data.List

import AMD64
import qualified Data.Set as S

type SymReg     = String
type RealReg    = String

type Vtable = [] (SymReg, RealReg)

data RegEnv = RegEnv { instCount           :: Int,
                       registerPriority    :: [] RealReg,
                       numOfCallerSaveRegs :: Int,
                       livenessInSets      :: [] (S.Set SymReg)
                     }
             deriving Show
type RegAlloc a = State RegEnv a                      

data AMD64Function = AMD64Function { funPrologue :: [] AMD64.Instruction,
                                     funBody     :: [] AMD64.Instruction,
                                     funEpilogue :: [] AMD64.Instruction
                                   } deriving (Show)

data Edge   = Edge (SymReg, SymReg) deriving (Show)

data StackElement = StackElement SymReg ([] Edge)

instance Eq StackElement where
  StackElement sr1 (_) == StackElement sr2 _edges = sr1 == sr2

instance Ord StackElement where
  StackElement _ (edges1) <= StackElement _ (edges2) = length edges1 <= length edges2  

instance Eq Edge where
  Edge (sr1l,sr1r) == Edge (sr2l,sr2r) = (sr1l == sr2l) && (sr1r == sr2r) || (sr1l == sr2r) && (sr1r == sr2l)

instance Ord Edge where
  Edge (sr1l,sr1r) <= Edge (sr2l,sr2r) = (sr1l < sr2l) || (sr1l == sr2l) && (sr1r <= sr2r)

type Graph = (S.Set SymReg, S.Set Edge)

incr :: RegAlloc Int
incr = do
  regEnv <- get
  let i = instCount regEnv
  put regEnv {instCount = i + 1}
  return i

setCode :: AMD64.Instruction -> RegAlloc AMD64.Instruction
setCode = return

-- LIVENESS ANALYSIS BEGIN

regNames = ["rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp"] ++
  map ('r':) (map show [8..15])
filterDynamicRegs :: String -> Bool
filterDynamicRegs symReg = symReg `elem` regNames

edgeBelongsTo :: SymReg -> Edge -> Bool
edgeBelongsTo sreg (Edge (sr1,sr2)) = (sreg == sr1) || (sreg == sr2)

removeNodeAndEdges :: Graph -> SymReg -> Graph
removeNodeAndEdges (nodeSet, edgeSet) sreg =
  let
    newNodeSet = S.delete sreg nodeSet
    newEdgeSet = S.filter (not . edgeBelongsTo sreg) edgeSet
  in
    (newNodeSet, newEdgeSet)

noOfEdgesForNode :: Graph -> SymReg -> Int
noOfEdgesForNode (_, edgeSet) sreg =
  S.size $ S.filter (edgeBelongsTo sreg) edgeSet

numOfNodesInGraph :: Graph -> Int
numOfNodesInGraph (sregSet, _) = S.size sregSet

findLabelH :: Int -> String -> [] AMD64.Instruction -> Int
findLabelH n label (inst:insts) =
  case inst of
  AMD64.Label l ->  if (l == label) then n else findLabelH (n + 1) label insts
  _             -> findLabelH (n + 1) label insts
findLabelH _ label [] = error $ "Unable to locate label " ++ label

findLabel :: String -> [] AMD64.Instruction -> Int
findLabel label insts = findLabelH 0 label insts

getSuccessors :: [] AMD64.Instruction -> [] ([] Int)
getSuccessors insts = getSuccessorsH 0 insts insts
getSuccessorsH :: Int -> [] AMD64.Instruction -> [] AMD64.Instruction -> [] ([] Int)
getSuccessorsH n [] _                               = []
getSuccessorsH n (AMD64.JNE l : insts) allInsts  = [n + 1, findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (AMD64.JE l : insts) allInsts  = [n + 1, findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (AMD64.JZ l : insts) allInsts  = [n + 1, findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (AMD64.RET retaddr : insts) allInsts = [[]]
getSuccessorsH n (AMD64.JMP l : insts) allInsts  = [findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (_ : insts) allInsts    = [n + 1] : getSuccessorsH (n + 1) insts allInsts
-- call is caught by the latter pattern

generatedRegisters :: AMD64.Instruction -> S.Set SymReg
generatedRegisters inst = S.filter filterDynamicRegs $ generatedRegistersH inst

generatedRegistersH :: AMD64.Instruction -> S.Set AMD64.Reg
generatedRegistersH (AMD64.Label _)        = S.empty
generatedRegistersH (AMD64.Comment _)      = S.empty
generatedRegistersH (AMD64.MOV _ r1)       = S.singleton r1
generatedRegistersH (AMD64.CMP r0 r1)      = S.singleton r0 $ S.singleton r1
generatedRegistersH (AMD64.JMP _)          = S.empty
generatedRegistersH (AMD64.JNE _)          = S.empty
generatedRegistersH (AMD64.JZ _)           = S.empty
generatedRegistersH (AMD64.JE _)           = S.empty
generatedRegistersH (AMD64.INC reg)        = S.singleton reg
generatedRegistersH (AMD64.DEC reg)        = S.singleton reg
generatedRegistersH (AMD64.ADD r0 r1)      = S.insert r0 $ S.singleton r1
generatedRegistersH (AMD64.SUB r0 r1)      = S.insert r0 $ S.singleton r1
generatedRegistersH (AMD64.IMUL r0)        = S.singleton r0
generatedRegistersH (AMD64.MUL r0)         = S.singleton r0
generatedRegistersH (AMD64.IDIV r0)        = S.singleton r0
generatedRegistersH (AMD64.DIV r0)         = S.singleton r0
generatedRegistersH (AMD64.INT reg)        = S.empty
generatedRegistersH (AMD64.PUSH ret)       = S.singleton ret
generatedRegistersH (AMD64.POP ret)        = S.empty
generatedRegistersH (AMD64.RET _)          = S.empty
generatedRegistersH (AMD64.CALL _)         = S.empty

killRegister :: AMD64.Instruction -> S.Set AMD64.Reg
killRegister inst = S.filter filterDynamicRegs $ killRegisterH inst

killRegisterH :: AMD64.Instruction -> S.Set AMD64.Reg
killRegisterH (AMD64.Label _)        = S.empty
killRegisterH (AMD64.Comment _)      = S.empty
killRegisterH (AMD64.MOV r0 _)       = S.singleton r0
killRegisterH (AMD64.CMP r0 r1)      = S.empty
killRegisterH (AMD64.JMP _)          = S.empty
killRegisterH (AMD64.JNE _)          = S.empty
killRegisterH (AMD64.JZ _)           = S.empty
killRegisterH (AMD64.JE _)           = S.empty
killRegisterH (AMD64.INC reg)        = S.singleton reg
killRegisterH (AMD64.DEC reg)        = S.singleton reg
killRegisterH (AMD64.ADD r0 r1)      = S.singleton r0
killRegisterH (AMD64.SUB r0 r1)      = S.singleton r0
killRegisterH (AMD64.IMUL r0)        = S.singleton r0
killRegisterH (AMD64.MUL r0)         = S.singleton r0
killRegisterH (AMD64.IDIV r0)        = S.singleton r0
killRegisterH (AMD64.DIV r0)         = S.singleton r0
killRegisterH (AMD64.INT reg)        = S.empty
killRegisterH (AMD64.PUSH ret)       = S.empty
killRegisterH (AMD64.POP ret)        = S.singleton ret
killRegisterH (AMD64.RET _)          = S.empty
killRegisterH (AMD64.CALL _)         = S.empty

livenessInH :: Int ->
               [] (S.Set AMD64.Reg) ->
               [] (S.Set AMD64.Reg) ->
               [] ([] Int) ->
               [] (S.Set AMD64.Reg) ->
               [] (S.Set AMD64.Reg) ->
               [] (S.Set AMD64.Reg)
livenessInH i livenessInOld livenessInNew (sc:scs) (gl:gls) (kl:kls) =
  let pick x = if i <= x
               then livenessInOld !! (length livenessInOld - x -1)
               else livenessInNew !! (length livenessInNew - x -1)
      outI = S.unions $ map pick sc
  in
    livenessInH i livenessInOld ((livenessInNew ++ [gl `S.union` (outI `S.difference` kl)])) scs gls kls
livenessInH _ _ livenessInNew [] [] [] = livenessInNew

livenessIn :: [] (S.Set AMD64.Reg) ->
              [] ([] Int) ->
              [] (S.Set AMD64.Reg) ->
              [] (S.Set AMD64.Reg) ->
              [] (S.Set AMD64.Reg)
livenessIn livenessInPrev scs gls kls =
  let
    livenessNext = livenessInH 0 livenessInPrev [] scs gls kls
  in
    if livenessInPrev == livenessNext
    then reverse livenessInPrev
    else livenessIn livenessNext scs gls kls

livenessOut :: [] (S.Set AMD64.Reg) -> [] ([] Int) -> [] (S.Set AMD64.Reg)
livenessOut livenessIns successorss =
  let
    livenessOutH :: [] (S.Set AMD64.Reg) -> [] Int -> S.Set AMD64.Reg
    livenessOutH lvnis successors = S.unions $ map (\x -> lvnis !! x) successors
  in
    map (livenessOutH livenessIns) successorss    

getLiveness :: [] AMD64.Instruction -> ([] (S.Set AMD64.Reg), [] (S.Set AMD64.Reg), [] (S.Set AMD64.Reg), [] (S.Set AMD64.Reg))
getLiveness insts = 
  let
    scs            = getSuccessors insts
    gls            = map generatedRegisters insts
    kls            = map killRegister insts
    livenessInPrev = map (const S.empty) insts
    livenessInSet  = livenessIn (reverse livenessInPrev) (reverse scs) (reverse gls) (reverse kls)
    livenessOutSet = livenessOut livenessInSet scs
  in
    (livenessIn (reverse livenessInSet) (reverse scs) (reverse gls) (reverse kls), livenessOutSet, kls, gls)

getInterferenceSet :: [] (S.Set AMD64.Reg) -> [] (S.Set AMD64.Reg) -> S.Set Edge
getInterferenceSet livenessOuts killsets =
  let
    filterEdge :: Edge -> S.Set Edge
    filterEdge (Edge (a, b)) = if a == b
                               then S.empty
                               else if a < b
                                    then S.singleton (Edge (a, b))
                                    else S.singleton (Edge (b, a))
    removeRedundantTuplesFromSet :: S.Set Edge -> S.Set Edge
    removeRedundantTuplesFromSet edgeSet = S.unions $ map filterEdge (S.toList edgeSet)
    cartProd :: [] a -> [] b -> [] (a,b)
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]  
    addH :: (S.Set AMD64.Reg, S.Set AMD64.Reg) -> S.Set Edge
    addH (livenessOutSet, killSet) =
      let
        interferenceEdges =
          if (killSet == S.empty || livenessOutSet == S.empty)
          then Nothing
          else Just $ S.fromList $ map (\x -> Edge x) (cartProd (S.toList killSet) (S.toList livenessOutSet))
      in
        case interferenceEdges of
        Nothing -> S.empty
        Just p  -> removeRedundantTuplesFromSet p
  in S.unions $ map addH (zip livenessOuts killsets)

getSRegNames :: [] (S.Set AMD64.Reg) -> [] (S.Set AMD64.Reg) -> S.Set SymReg
getSRegNames kls gls =  S.filter filterDynamicRegs $ S.unions $ map (\(x,y) -> S.union x y) (zip kls gls)

-- Among the realregs, we should choose a real reg that maps to symregs which
-- do not interfere with the symreg in question
colorGraphSelect :: [] RealReg -> [] StackElement -> Vtable
colorGraphSelect rp = colorGraphSelectH []
  where
    colorGraphSelectH :: Vtable -> [] StackElement -> [] (SymReg, RealReg)
    colorGraphSelectH _ []              = []
    colorGraphSelectH vt (selem:selems') = newMapping : colorGraphSelectH (newMapping:vt) selems'
        where
           newMapping = choose rp vt selem
    choose :: [] RealReg -> Vtable -> StackElement -> (SymReg, RealReg)
    choose rregs vt (StackElement node edges) =
      case filter (regIsCompatible edges vt) rregs of
      rr:_ -> (node, rr)
      _    -> (node, "SPILLED") -- spill vars if no color available
    regIsCompatible :: [] Edge -> Vtable -> RealReg -> Bool
    regIsCompatible edges vt rr = case inverseLookup vt rr of
      []      -> True
      sregs   -> all (not . nodeInEdges edges) sregs
    nodeInEdges :: [] Edge -> SymReg -> Bool
    nodeInEdges edges sreg = any (edgeBelongsTo sreg) edges
    inverseLookup :: Vtable -> RealReg -> [] SymReg
    inverseLookup ((_,"SPILLED"):vt) rreg = inverseLookup vt rreg --Ignore interf of spilled vars.
    inverseLookup ((sr,rr):vt) rreg = if rr == rreg
                                    then sr:(inverseLookup vt rreg)
                                    else inverseLookup vt rreg
    inverseLookup [] _ = []


colorGraphSimplify ::  [] RealReg -> Graph -> [] StackElement
colorGraphSimplify rp graph =
  let
    lengthRP = length rp
    removeNodes :: Int -> Graph -> [] StackElement
    removeNodes nr oldGraph = case (S.null (fst oldGraph)) of
      True            -> []
      False           -> if (nextGraph == oldGraph && numOfNodesInGraph nextGraph > 0)
                         then error "Unpossible1!" 
                         else stackElem : (removeNodes nr nextGraph)
      where
        (nextGraph, stackElem) = removeNodesWithFewEdgesH nr oldGraph
        removeNodesWithFewEdgesH :: Int -> Graph -> (Graph, StackElement)
        removeNodesWithFewEdgesH nR' graph' = 
          let
            (sregSet', edgeSet) = graph'
            edges'              = S.toList edgeSet
            sregs'              = S.toList sregSet'
            srToNumOfEdges'     = map (\x -> (x, noOfEdgesForNode graph' x)) sregs'
            nodesToRemove       = map fst $ filter (\x -> (snd x < nR')) srToNumOfEdges'
          in
            case (nodesToRemove) of
            -- Here, we remove just one node, not all
            (node':_) -> (removeNodeAndEdges graph' node',
                          StackElement node' (filter (edgeBelongsTo node') edges'))
            _         -> case srToNumOfEdges' of -- TÆM p. 168 has a heuristic for this case, 
              (node',_):_ -> (removeNodeAndEdges graph' node', StackElement node' (filter (edgeBelongsTo node') edges'))
              _           -> error "Unpossible2!" -- should not happen due to check in above if expr. ( ... > 0)
  in
    reverse $ removeNodes lengthRP graph

colorGraph :: [] RealReg -> Graph -> [] (SymReg, RealReg)  
colorGraph rp graph =
  let
    (sregSet, _)           = graph
    sregs                  = S.toList sregSet
    lengthRP               = length rp
    srToNumOfEdges         = map (\x -> (x, noOfEdgesForNode graph x)) sregs
    numOfRegs              = length srToNumOfEdges
  in
    if numOfRegs <= lengthRP
    then zip sregs rp
    else colorGraphSelect rp $ colorGraphSimplify rp graph

-- LIVENESS ANALYSIS END


-- Main function of this module
regAllocInsts :: Int -> [] RealReg -> AMD64Function -> [] AMD64.Instruction
regAllocInsts ncsr rp function =
  let
    pl  = funPrologue function
    bd  = funBody function
    epl = funEpilogue function
  in
    pl ++ regAllocInstsH (length epl) 0 ncsr rp (bd ++ epl) ++ epl

-- Should this accept an input of type AMD64Function instead?
regAllocInstsH :: Int -> Int -> Int -> [] RealReg -> [] AMD64.Instruction -> [] AMD64.Instruction
regAllocInstsH epilogueLength numOfSpilledRegs numOfCallerSaveRegs' rp insts =
  let
    spilledVarsIn vt = elem "SPILLED" (map snd vt)
    (livInSets, livOutSets, killSets, generatedSets) = getLiveness insts
    interferenceSet = getInterferenceSet livOutSets killSets
    sRegNames = getSRegNames killSets generatedSets
    vtable = colorGraph rp (sRegNames, interferenceSet)
  in
    if (spilledVarsIn vtable)
    then
      let
        vtableSpill = zip (map fst (filter (\x -> (snd x)== "SPILLED") vtable)) (map show [1..])
        numOfSpilledRegs' = numOfSpilledRegs + (length vtableSpill) 
        insts'  = evalState (liftM concat (mapM (regAllocInst True vtableSpill ) insts)) (RegEnv 0 [] 0 [])
      in
        -- Put in check for infinite recursion here.
        regAllocInstsH epilogueLength numOfSpilledRegs' numOfCallerSaveRegs' rp insts'
    else
      let
        removedEpilogue = take (length insts - epilogueLength) insts
        popSpilledRegs  = [AMD64.ADD "rsp" (show (8*numOfSpilledRegs))] -- hardcoding "8" sucks
        pushSpilledRegs = [AMD64.ADD "rsp" (show (-8*numOfSpilledRegs))] -- ceil to nearest 16?
        storeCalleeSave = evalState (storeCalleeSaveRegs vtable) (RegEnv 0 rp numOfCallerSaveRegs' [])
        loadCalleeSave  = evalState (loadCalleeSaveRegs vtable) (RegEnv 0 rp numOfCallerSaveRegs' [])
        regAllocCode    = evalState
                          (liftM concat (mapM (regAllocInst False vtable) removedEpilogue))
                          (RegEnv 0 rp numOfCallerSaveRegs' livInSets)
      in
        storeCalleeSave ++ pushSpilledRegs ++ regAllocCode ++ popSpilledRegs ++ loadCalleeSave
        

-- Slightly messy
-- Used for both register allocation and for spill-rewriting 
mapReg :: Vtable -> SymReg -> RegAlloc String
mapReg ((sr,rc:rcs):l) sreg =
  if (sreg `elem` regNames)
     then return $ sreg
  else
    if sr == sreg
    then
      if isNumber rc
      then do -- only used for spilling-rewrites
        regEnv <- get
        let i = instCount regEnv
        return $ sr ++ show i
      else
        return $ rc:rcs
    else
      mapReg l sreg
mapReg [] sreg = return sreg -- This case is used for spilling rewriting only -- error otherwise

getAddressOfSpilledReg :: SymReg -> Vtable -> String
getAddressOfSpilledReg sreg ((symr,spillr):l) = if sreg == symr
-- Not very elegant. 8 because byte addressing.
                                        then show (8*((read spillr :: Int) - 1))
                                        else getAddressOfSpilledReg sreg l
getAddressOfSpilledReg sreg []                = error $ "getAddressOfSpilledReg failed with sreg = " ++ show sreg                                   

-- This should be calculating addresses in AMD64 assembly. How is that done?
-- Compare to MIPS solution.
spillWrap :: Bool -> Vtable -> String -> String -> String -> String -> String -> String ->
             ([] AMD64.Instruction, [] AMD64.Instruction, [] AMD64.Instruction)
spillWrap spillingRewrite vtable targetr sr1 sr2 targetrRes sr1Res sr2Res =
  let
    post = if targetr /= targetrRes 
           then [AMD64.MOV ('[' : "rsp +" (getAddressOfSpilledReg targetr vtable)++"]") targetrRes]
           else []
    pre1 = if sr1 /= sr1Res
           then [AMD64.MOV sr1Res ('[' : "rsp +" (show (getAddressOfSpilledReg sr1 vtable))) ++ "]"]
           else []
    pre2 = if sr2 /= sr2Res
           then [AMD64.MOV sr2Res ('[' : "rsp +" (show (getAddressOfSpilledReg sr2 vtable))) ++ "]"]
           else []
  in
    if spillingRewrite
    then (pre1, pre2, post)
    else ([], [], [])


regIsCallerSave :: RealReg -> RegAlloc Bool
regIsCallerSave "$ra" = return True
regIsCallerSave "$v0" = return True
regIsCallerSave rr = do
  regEnv <- get
  let
    rp   = registerPriority regEnv
    rnum = elemIndex rr rp
    ncsr = numOfCallerSaveRegs regEnv
    in
    case rnum of
    Just num -> return $ num < ncsr
    Nothing -> error $ "Argument " ++ rr ++  " to regIsCallerSave was not found in register priority list."

regIsCalleeSave :: RealReg -> RegAlloc Bool
regIsCalleeSave rr = do
  b <- regIsCallerSave rr
  return $ not b

-- Make storeLive and loadLive could prob. be made into one function
storeRegistersCode :: Int -> Int -> [] AMD64.Reg -> [] AMD64.Instruction
storeRegistersCode n i (reg:regs) = (AMD64.SW reg "$sp" (show (4*n - 4*(i+1)))) : (storeRegistersCode n (i + 1) regs)
storeRegistersCode _ _ []         = []

loadRegistersCode :: Int -> Int -> [] AMD64.Reg -> [] AMD64.Instruction
loadRegistersCode n i (reg:regs) = (AMD64.LW reg "$sp" (show (4*n - 4*(i+1)))) : (loadRegistersCode n (i + 1) regs)
loadRegistersCode _ _ []         = []
  
storeLive :: [] RealReg -> RegAlloc ([] AMD64.Instruction)
storeLive stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ AMD64.ADDI "$sp" "$sp" ('-' : (show (4*n)))
    callerSaveRegs <- filterM regIsCallerSave stRegs
    let stRegsCode = storeRegistersCode n 0 callerSaveRegs
    return $ mvSP : stRegsCode

loadLive :: [] RealReg -> RegAlloc ([] AMD64.Instruction)
loadLive stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ AMD64.ADDI "$sp" "$sp" (show (4*n))
    callerSaveRegs <- filterM regIsCallerSave stRegs
    let stRegsCode = loadRegistersCode n 0 callerSaveRegs
    return $ stRegsCode ++ [mvSP]

storeLive2 :: [] RealReg -> RegAlloc ([] AMD64.Instruction)
storeLive2 stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ AMD64.ADDI "$sp" "$sp" ('-' : (show (4*n)))
    let stRegsCode = storeRegistersCode n 0 stRegs
    return $ mvSP : stRegsCode

loadLive2 :: [] RealReg -> RegAlloc ([] AMD64.Instruction)
loadLive2 stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ AMD64.ADDI "$sp" "$sp" (show (4*n))
    let stRegsCode = loadRegistersCode n 0 stRegs
    return $ stRegsCode ++ [mvSP]    

-- given the vtable that has been calculated, we should have a function that
-- stores the callee-save registers which are used by the vtable.

storeCalleeSaveRegs :: Vtable -> RegAlloc ([] AMD64.Instruction)
storeCalleeSaveRegs vtable = do
  stRegs <- filterM regIsCalleeSave $ S.toList $ S.fromList (map snd vtable)
  storeLive2 stRegs

loadCalleeSaveRegs :: Vtable -> RegAlloc ([] AMD64.Instruction)
loadCalleeSaveRegs vtable = do
  stRegs <- filterM regIsCalleeSave $ S.toList $ S.fromList (map snd vtable)
  loadLive2 stRegs

regAllocInst :: Bool -> Vtable -> AMD64.Instruction -> RegAlloc ([] AMD64.Instruction)
regAllocInst _spillingRewrite vtable (AMD64.Label l) = do
  incr
  return [AMD64.Label l]
regAllocInst spillingRewrite vtable (AMD64.Comment s) = do
  incr
  return [AMD64.Comment s]
regAllocInst spillingRewrite vtable (AMD64.XOR rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.XOR rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (AMD64.XORI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.XORI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (AMD64.ADD rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.ADD rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (AMD64.SUB rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.SUB rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (AMD64.MUL rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.MUL rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (AMD64.DIV rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.DIV rdRes rsRes rtRes] ++ post1  
regAllocInst spillingRewrite vtable (AMD64.ADDI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.ADDI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (AMD64.SUBI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.SUBI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (AMD64.SW rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.SW rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (AMD64.LW rs rt imm) = do -- Is this one correct?
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.LW rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (AMD64.BNE rs rt l) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, _) = spillWrap spillingRewrite vtable "" rs rt "" rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [AMD64.BNE rsRes rtRes l]
regAllocInst spillingRewrite vtable  (AMD64.JR rd) = do
  rdRes <- mapReg vtable rd
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable "" rd "" "" rdRes ""
  incr
  return $ pre1 ++ pre2 ++ [AMD64.JR rdRes] ++ post1
regAllocInst _ _ (AMD64.J l) = do
  incr
  return [AMD64.J l]
regAllocInst spillingRewrite vtable (AMD64.JAL l) = do-- Here the store and load stuff should be handled. Potentially in a monade?
  regEnv <- get
  i <- incr
  let sliveRegs = S.toList $ (livenessInSets regEnv !! i)
  rliveRegs <- mapM (mapReg vtable) sliveRegs
  store <- storeLive (rliveRegs ++ ["$ra"])
  load  <- loadLive $ rliveRegs  ++ ["$ra"]
  if spillingRewrite
    then return [AMD64.JAL l]
    else return $ store ++ [AMD64.JAL l] ++ load

