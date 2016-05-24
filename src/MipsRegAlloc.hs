{-# LANGUAGE TemplateHaskell #-}
module MipsRegAlloc where

import Control.Monad.State.Lazy
import Data.Char
import Data.List

import Mips
import qualified Data.Set as S

type SymReg     = String
type RealReg    = String

type Vtable = [] (SymReg, RealReg)

data RegEnv = RegEnv { instCount           :: Int,
                       registerPriority    :: [] RealReg,
                       numOfCallerSaveRegs :: Int,
                       livenessInSets      :: [] (S.Set Mips.Reg),
                       numOfBytesSPShifted :: Int,
                       liveRegsStored      :: [] RealReg
                     }
             deriving Show
type RegAlloc a = State RegEnv a                      

data MipsFunction = MipsFunction { funPrologue :: [] Mips.Instruction,
                                   funLoadRegs :: [] Mips.Instruction,
                                   funBody     :: [] Mips.Instruction,
                                   funEpilogue :: [] Mips.Instruction
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

storeSPShift :: Int -> RegAlloc Int
storeSPShift n = do
  regEnv <- get
  let sps    = numOfBytesSPShifted regEnv
  put regEnv { numOfBytesSPShifted = sps + n }
  return $ sps + n

setLiveRegsStored :: [] RealReg -> RegAlloc ()
setLiveRegsStored rrs = do
  regEnv <- get
  put regEnv { liveRegsStored = rrs }
  return ()

getLiveRegsStored :: RegAlloc ([] RealReg)
getLiveRegsStored = do
  regEnv <- get
  return $ liveRegsStored regEnv

resetLiveRegsStored :: RegAlloc ()
resetLiveRegsStored = do
  regEnv <- get
  put regEnv { liveRegsStored = [] }

incr :: RegAlloc Int
incr = do
  regEnv <- get
  let i = instCount regEnv
  put regEnv {instCount = i + 1}
  return i

setCode :: Mips.Instruction -> RegAlloc Mips.Instruction
setCode = return

-- LIVENESS ANALYSIS BEGIN

filterDynamicRegs :: String -> Bool
filterDynamicRegs ('$':_)     = False
filterDynamicRegs (_)         = True

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

findLabelH :: Int -> String -> [] Mips.Instruction -> Int
findLabelH n label (inst:insts) =
  case inst of
  Mips.Label l ->  if (l == label) then n else findLabelH (n + 1) label insts
  _            -> findLabelH (n + 1) label insts
findLabelH _ label [] = error $ "Unable to locate label " ++ label

findLabel :: String -> [] Mips.Instruction -> Int
findLabel label insts = findLabelH 0 label insts

getSuccessors :: [] Mips.Instruction -> [] ([] Int)
getSuccessors insts = getSuccessorsH 0 insts insts

-- Atm a function must end with JR but this can be fixed by imposing the border
-- condition that the last instruction returns the empty set.
getSuccessorsH :: Int -> [] Mips.Instruction -> [] Mips.Instruction -> [] ([] Int)
getSuccessorsH n [] _                               = []
getSuccessorsH n (Mips.BNE _ _ l : insts) allInsts  = [n + 1, findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (Mips.JR retaddr : insts) allInsts = [[]]
getSuccessorsH n (Mips.J l : insts) allInsts  = [findLabel l allInsts] : getSuccessorsH (n + 1) insts allInsts
getSuccessorsH n (_ : insts) allInsts    = [n + 1] : getSuccessorsH (n + 1) insts allInsts
-- JAL is covered by the last catch-all rule
-- DEVFIX: Making general rules is dangerous since it may miss functions with special
-- rules that have not been implemented yet.

generatedRegisters :: Mips.Instruction -> S.Set Mips.Reg
generatedRegisters inst = S.filter filterDynamicRegs $ generatedRegistersH inst


generatedRegistersH :: Mips.Instruction -> S.Set Mips.Reg
generatedRegistersH (Mips.Label _)        = S.empty
generatedRegistersH (Mips.Comment _)      = S.empty
generatedRegistersH (Mips.XOR _rd rs rt)  = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.XORI _rd rs _)  = S.singleton rs
generatedRegistersH (Mips.ADD _rd rs rt)  = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.ADDI _rd rs _)  = S.singleton rs
generatedRegistersH (Mips.SUB _rd rs rt)  = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.SUBI _rs rt _)  = S.singleton rt
generatedRegistersH (Mips.MUL _rd rs rt)  = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.DIV _rd rs rt)  = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.JR reg)         = S.singleton reg
generatedRegistersH (Mips.LW _ rt _)      = S.singleton rt
generatedRegistersH (Mips.SW rs rt _)     = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.JAL _)          = S.empty
generatedRegistersH (Mips.BNE rs rt _imm) = S.insert rs $ S.singleton rt
generatedRegistersH (Mips.J l)            = S.empty

killRegister :: Mips.Instruction -> S.Set Mips.Reg
killRegister inst = S.filter filterDynamicRegs $ killRegisterH inst

killRegisterH :: Mips.Instruction -> S.Set Mips.Reg
killRegisterH (Mips.Label _)       = S.empty
killRegisterH (Mips.Comment _)     = S.empty
killRegisterH (Mips.XOR rd _ _ )   = S.singleton rd
killRegisterH (Mips.XORI rd _ _ )  = S.singleton rd
killRegisterH (Mips.ADD rd _ _ )   = S.singleton rd
killRegisterH (Mips.ADDI rd _ _ )  = S.singleton rd
killRegisterH (Mips.SUB rd _ _ )   = S.singleton rd
killRegisterH (Mips.SUBI rs _ _ )  = S.singleton rs
killRegisterH (Mips.MUL rd _ _ )   = S.singleton rd
killRegisterH (Mips.DIV rd _ _ )   = S.singleton rd
killRegisterH (Mips.JR _ )         = S.empty
killRegisterH (Mips.LW rs _ _ )    = S.singleton rs
killRegisterH (Mips.SW _ _ _ )     = S.empty
killRegisterH (Mips.JAL _)         = S.empty
killRegisterH (Mips.BNE _ _ _)     = S.empty
killRegisterH (Mips.J l)           = S.empty

livenessInH :: Int ->
               [] (S.Set Mips.Reg) ->
               [] (S.Set Mips.Reg) ->
               [] ([] Int) ->
               [] (S.Set Mips.Reg) ->
               [] (S.Set Mips.Reg) ->
               [] (S.Set Mips.Reg)
livenessInH i livenessInOld livenessInNew (sc:scs) (gl:gls) (kl:kls) =
  let pick x = if i <= x
  -- Prelude.!!: index too large. The following instruction fails when more args in function than regs
               then livenessInOld !! (length livenessInOld - x -1)
               else livenessInNew !! (length livenessInNew - x -1)
      outI = S.unions $ map pick sc
  in
    livenessInH i livenessInOld ((livenessInNew ++ [gl `S.union` (outI `S.difference` kl)])) scs gls kls
livenessInH _ _ livenessInNew [] [] [] = livenessInNew

livenessIn :: [] (S.Set Mips.Reg) ->
              [] ([] Int) ->
              [] (S.Set Mips.Reg) ->
              [] (S.Set Mips.Reg) ->
              [] (S.Set Mips.Reg)
livenessIn livenessInPrev scs gls kls =
  let
    livenessNext = livenessInH 0 livenessInPrev [] scs gls kls
  in
    if livenessInPrev == livenessNext
    then reverse livenessInPrev
    else livenessIn livenessNext scs gls kls

livenessOut :: [] (S.Set Mips.Reg) -> [] ([] Int) -> [] (S.Set Mips.Reg)
livenessOut livenessIns successorss =
  let
    livenessOutH :: [] (S.Set Mips.Reg) -> [] Int -> S.Set Mips.Reg
    livenessOutH lvnis successors = S.unions $ map (\x -> lvnis !! x) successors
  in
    map (livenessOutH livenessIns) successorss    

getLiveness :: [] Mips.Instruction -> ([] (S.Set Mips.Reg), [] (S.Set Mips.Reg), [] (S.Set Mips.Reg), [] (S.Set Mips.Reg))
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

getInterferenceSet :: [] (S.Set Mips.Reg) -> [] (S.Set Mips.Reg) -> S.Set Edge
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
    addH :: (S.Set Mips.Reg, S.Set Mips.Reg) -> S.Set Edge
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

getSRegNames :: [] (S.Set Mips.Reg) -> [] (S.Set Mips.Reg) -> S.Set SymReg
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
regAllocInsts :: Int -> [] RealReg -> MipsFunction -> [] Mips.Instruction
regAllocInsts ncsr rp function =
  let
    pl  = funPrologue function
    lr  = funLoadRegs function
    bd  = funBody function
    epl = funEpilogue function
  in
    pl ++ regAllocInstsH (length epl) 0 0 ncsr rp lr (bd ++ epl) ++ epl

-- Should this accept an input of type MipsFunction instead?
regAllocInstsH :: Int -> Int -> Int -> Int -> [] RealReg -> [] Mips.Instruction -> [] Mips.Instruction -> [] Mips.Instruction
regAllocInstsH epilogueLength numOfSpilledRegs lrShiftBytes numOfCallerSaveRegs' rp loadRegsInsts insts =
  let
    spilledVarsIn vt = elem "SPILLED" (map snd vt)
    (livInSets, livOutSets, killSets, generatedSets) = getLiveness (loadRegsInsts ++ insts)
    interferenceSet = getInterferenceSet livOutSets killSets
    sRegNames = getSRegNames killSets generatedSets
    vtable = colorGraph rp (sRegNames, interferenceSet)
  in
    if (spilledVarsIn vtable)
    then
      let
        vtableSpill = zip (map fst (filter (\x -> (snd x)== "SPILLED") vtable)) (map show [1..])
        numOfSpilledRegs' = numOfSpilledRegs + (length vtableSpill) 
        insts'  = evalState (liftM concat (mapM (regAllocInst True vtableSpill ) (loadRegsInsts ++ insts))) (RegEnv 0 [] 0 [] 0 [])
        lrShiftBytes' = 4*numOfSpilledRegs' + lrShiftBytes -- could be handled better in a monad, maybe.
      in
        -- Put in check for infinite recursion here.
        regAllocInstsH epilogueLength numOfSpilledRegs' lrShiftBytes' numOfCallerSaveRegs' rp loadRegsInsts insts'
    else
      let
        removedEpilogue     = take (length insts - epilogueLength) insts
        popSpilledRegs      = [Mips.ADDI "$sp" "$sp" (show (4*numOfSpilledRegs))] -- hardcoding "4" sucks
        pushSpilledRegs     = [Mips.ADDI "$sp" "$sp" (show (-4*numOfSpilledRegs))]
        storeCalleeSave     = evalState (storeCalleeSaveRegs vtable) (RegEnv 0 rp numOfCallerSaveRegs' [] 0 [])
        numOfCalleeSaveRegs = length $ evalState (getCalleeSaveRegs vtable) (RegEnv 0 rp numOfCallerSaveRegs' [] 0 [])
        loadCalleeSave      = evalState (loadCalleeSaveRegs vtable) (RegEnv 0 rp numOfCallerSaveRegs' [] 0 [])
          -- lrShiftBytes is number of spilled regs plus number of stored calleeSave registers plus the
          -- number of stored callee save registers. Right?
          -- The question then is, how do we get the latter?
          -- I could write a wrapper function around regAllocCode that returned this.
          -- Not clear how regAllocInst should return information about how many
          -- bytes that the regAlloc upon meeting a JAL has moved the SP.
          -- The information probably has to be put into the RegEnv monade.
          -- That will be the 1st strategy!
        rewrittenlrRegs     = lrShift (lrShiftBytes + numOfCalleeSaveRegs) loadRegsInsts
        -- It all takes place within the same monade!
        -- So I only need to extract the number in the end, right?
        -- I need my own evalState. It must return the list of instructions as well as the 
        regAllocCode        = evalState
                                  (liftM concat (mapM (regAllocInst False vtable) (rewrittenlrRegs ++ removedEpilogue)))
                                  (RegEnv 0 rp numOfCallerSaveRegs' livInSets 0 [])
      in
        storeCalleeSave ++ pushSpilledRegs ++
        regAllocCode ++ popSpilledRegs ++ loadCalleeSave

lrShift :: Int -> [] Mips.Instruction -> [] Mips.Instruction
lrShift lrShiftBytes ((Mips.LW reg "$sp" amount):insts) =
  Mips.LW reg "$sp" (show ((read amount :: Int) + lrShiftBytes)) :
  (lrShift lrShiftBytes insts)
lrShift _ inst                                          = inst
        

-- Used for both register allocation and for spill-rewriting 
mapReg :: Vtable -> SymReg -> RegAlloc String
mapReg _ ('$':rn) = do return $ '$':rn
mapReg ((sr,rc:rcs):l) sreg =
  if sr == sreg
  then if isNumber rc
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
-- Not very elegant. 4 because byte addressing.                                             
                                        then show (4*((read spillr :: Int) - 1))
                                        else getAddressOfSpilledReg sreg l
getAddressOfSpilledReg sreg []                = error $ "getAddressOfSpilledReg failed with sreg = " ++ show sreg                                   

spillWrap :: Bool -> Vtable -> String -> String -> String -> String -> String -> String ->
             ([] Mips.Instruction, [] Mips.Instruction, [] Mips.Instruction)
spillWrap spillingRewrite vtable targetr sr1 sr2 targetrRes sr1Res sr2Res =
  let
    post = if targetr /= targetrRes 
           then [Mips.SW targetrRes "$sp" (getAddressOfSpilledReg targetr vtable)]
           else []
    pre1 = if sr1 /= sr1Res
           then [Mips.LW sr1Res "$sp" (getAddressOfSpilledReg sr1 vtable)]
           else []
    pre2 = if sr2 /= sr2Res
           then [Mips.LW sr2Res "$sp" (getAddressOfSpilledReg sr2 vtable)] 
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
storeRegistersCode :: Int -> Int -> [] Mips.Reg -> [] Mips.Instruction
storeRegistersCode n i (reg:regs) = (Mips.SW reg "$sp" (show (4*n - 4*(i+1)))) : (storeRegistersCode n (i + 1) regs)
storeRegistersCode _ _ []         = []

loadRegistersCode :: Int -> Int -> [] Mips.Reg -> [] Mips.Instruction
loadRegistersCode n i (reg:regs) = (Mips.LW reg "$sp" (show (4*n - 4*(i+1)))) : (loadRegistersCode n (i + 1) regs)
loadRegistersCode _ _ []         = []
  
storeLive :: [] RealReg -> RegAlloc ([] Mips.Instruction)
storeLive stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ Mips.ADDI "$sp" "$sp" ('-' : (show (4*n)))
    callerSaveRegs <- filterM regIsCallerSave stRegs
    let stRegsCode = storeRegistersCode n 0 callerSaveRegs
    return $ mvSP : stRegsCode

loadLive :: [] RealReg -> RegAlloc ([] Mips.Instruction)
loadLive stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ Mips.ADDI "$sp" "$sp" ((show (4*n)))    
    callerSaveRegs <- filterM regIsCallerSave stRegs
    let stRegsCode = loadRegistersCode n 0 callerSaveRegs
    return $ stRegsCode ++ [ mvSP ]

storeLive2 :: [] RealReg -> RegAlloc ([] Mips.Instruction)
storeLive2 stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ Mips.ADDI "$sp" "$sp" ('-' : (show (4*n)))
    let stRegsCode = storeRegistersCode n 0 stRegs
    return $ mvSP : stRegsCode

loadLive2 :: [] RealReg -> RegAlloc ([] Mips.Instruction)
loadLive2 stRegs =
  let n      = length stRegs
  in do
    mvSP <- setCode $ Mips.ADDI "$sp" "$sp" (show (4*n))
    let stRegsCode = loadRegistersCode n 0 stRegs
    return $ stRegsCode ++ [mvSP]    

-- given the vtable that has been calculated, we should have a function that
-- stores the callee-save registers which are used by the vtable.

getCalleeSaveRegs :: Vtable -> RegAlloc ([] RealReg)
getCalleeSaveRegs vt = do
  stRegs <- filterM regIsCalleeSave $ S.toList $ S.fromList (map snd vt)
  return stRegs

storeCalleeSaveRegs :: Vtable -> RegAlloc ([] Mips.Instruction)
storeCalleeSaveRegs vt = do
  stRegs <- getCalleeSaveRegs vt
  storeLive2 stRegs

loadCalleeSaveRegs :: Vtable -> RegAlloc ([] Mips.Instruction)
loadCalleeSaveRegs vtable = do
  stRegs <- filterM regIsCalleeSave $ S.toList $ S.fromList (map snd vtable)
  loadLive2 stRegs

regAllocInst :: Bool -> Vtable -> Mips.Instruction -> RegAlloc ([] Mips.Instruction)
regAllocInst _spillingRewrite vtable (Mips.Label l) = do
  incr
  return [Mips.Label l]
regAllocInst spillingRewrite vtable (Mips.XOR rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.XOR rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (Mips.XORI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.XORI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (Mips.ADD rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.ADD rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (Mips.SUB rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.SUB rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (Mips.MUL rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.MUL rdRes rsRes rtRes] ++ post1
regAllocInst spillingRewrite vtable (Mips.DIV rd rs rt) = do
  rdRes <- mapReg vtable rd
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rd rs rt rdRes rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.DIV rdRes rsRes rtRes] ++ post1  
regAllocInst spillingRewrite vtable (Mips.ADDI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.ADDI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (Mips.SUBI rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.SUBI rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (Mips.SW rs rt imm) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.SW rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (Mips.LW rs rt imm) = do -- Is this one correct?
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable rs rt "" rsRes rtRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.LW rsRes rtRes imm] ++ post1
regAllocInst spillingRewrite vtable (Mips.BNE rs rt l) = do
  rsRes <- mapReg vtable rs
  rtRes <- mapReg vtable rt
  let (pre1, pre2, _) = spillWrap spillingRewrite vtable "" rs rt "" rsRes rtRes
  incr
  return $ pre1 ++ pre2 ++ [Mips.BNE rsRes rtRes l]
regAllocInst spillingRewrite vtable  (Mips.JR rd) = do
  rdRes <- mapReg vtable rd
  let (pre1, pre2, post1) = spillWrap spillingRewrite vtable "" rd "" "" rdRes ""
  incr
  return $ pre1 ++ pre2 ++ [Mips.JR rdRes] ++ post1
regAllocInst _ _ (Mips.J l) = do
  incr
  return [Mips.J l]
  -- The below case causes errors in the loading of registers from the stack
  -- since the it wraps the lw instructions in store/load instruction lists
  -- and they may may move the stack pointer. The solution is most likely to
  -- handle the loadRegister functions seperately thus giving them

regAllocInst spillingRewrite vtable (Mips.JAL l) = do
  i <- incr
  return [Mips.JAL l]
regAllocInst spillingRewrite vtable (Mips.Comment "FunCallStart") = do
  regEnv <- get
  i <- incr
  let sliveRegs = S.toList $ (livenessInSets regEnv !! i)
  rliveRegs <- mapM (mapReg vtable) sliveRegs
  let rRegsToStore = rliveRegs ++ ["$ra"]
  store <- storeLive rRegsToStore
  setLiveRegsStored rRegsToStore  -- needed to remember how much to move SP after funcall
  if spillingRewrite
    then return [ Mips.Comment "FunCallStart" ]
    else return $ store ++ [ Mips.Comment "FunCallStart" ]
regAllocInst spillingRewrite vtable (Mips.Comment "FunCallEnd") = do
  regEnv <- get
  incr
  rRegsToLoad <- getLiveRegsStored
  resetLiveRegsStored
  load <- loadLive rRegsToLoad
  if spillingRewrite
    then return [ Mips.Comment "FunCallEnd" ]
    else return $ Mips.Comment "FunCallEnd" : load
regAllocInst spillingRewrite vtable (Mips.Comment s) = do
  incr
  return [Mips.Comment s]
