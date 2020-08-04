{-# OPTIONS_HADDOCK hide #-}
module ForSyDe.Atom.MoC.SDF.SADFDemo where

import ForSyDe.Atom
import ForSyDe.Atom.MoC (takeS)
import ForSyDe.Atom.MoC.SDF.SADF
import ForSyDe.Atom.Skel.Vector as V

type StateDEC = Int      -- state decoder
type Instr = (Op, [Arg]) -- instruction type (operation, args)
type Arg = Int           -- numerical arguments
type Op  = String        -- operation name
type PC  = Int           -- program counter
type Reg = Arg           -- register

---------------------------------------------------------
-- Instruction Fetch (IF) Kernel
---------------------------------------------------------

type ScenarioIF = ( (Cons, Cons), (Prod, Prod, Prod)
                  , [Arg] -> [PC] -> ([Op], [[Arg]], [PC]) )

ifScenario :: StateDEC -> ScenarioIF
ifScenario n
  | n > 9 || n < 0  = error "ifScenario: Non existent scenario"
  | n >= 6 && n < 9 = -- branch
    ((1,1), (1,1,1), \[a] [pc] -> ([op program (pc+a)], [arg program (pc+a)], [pc+a+1])) 
  | otherwise       = -- no branch
    ((0,1), (1,1,1), \_   [pc] -> ([op program pc],     [arg program pc],     [pc+1]))
  where arg mem i = snd $ mem <@! i
        op  mem i = fst $ mem <@! i

-- | 'ifKernel' is the Instruction Fetch (IF) kernel process
ifKernel :: Signal ScenarioIF -> Signal Arg -> (Signal Op, Signal [Arg])
ifKernel ifCt sigBr = (sigOp, sigArg)
  where (sigOp, sigArg, sigPc) = kernel23 ifCt sigBr sigPc'
        sigPc' = delay [1] sigPc

---------------------------------------------------------
-- Execute (EXE) Kernel
---------------------------------------------------------

type ScenarioEXE = ( (Cons, Cons), (Prod, Prod, Prod)
                   , [[Arg]] -> [Vector Reg] -> ([Arg], [Arg], [Vector Reg]))

-- | 'exeScenario' is the list of possible scenarios for the EXE kernel
exeScenario :: StateDEC -> ScenarioEXE
exeScenario 0 = ((1,0), (0,0,0), \_ _ -> ([], [], []))                                                  -- nop
exeScenario 1 = ((1,1), (0,0,1), \[[rd, rs]] [r] -> ([], [], [replace rd (r <@! rs) r]))                -- mov
exeScenario 2 = ((1,1), (0,0,1), \[[rd, i]]  [r] -> ([], [], [replace rd i r]))                         -- movi
exeScenario 3 = ((1,1), (0,0,1), \[[rd, rs]] [r] -> ([], [], [replace rd ((r <@! rd) + (r <@! rs)) r])) -- add
exeScenario 4 = ((1,1), (0,0,1), \[[rd, rs]] [r] -> ([], [], [replace rd ((r <@! rd) - (r <@! rs)) r])) -- sub
exeScenario 5 = ((1,1), (0,0,1), \[[rd, rs]] [r] -> ([], [], [replace rd ((r <@! rd) * (r <@! rs)) r])) -- mul
exeScenario 6 = ((1,1), (1,0,1), \[[rs, v]]  [r] -> ([if r <@! rs == 0 then v else 0], [], [r]))        -- bez
exeScenario 7 = ((1,1), (1,0,1), \[[rs, v]]  [r] -> ([if r <@! rs > 0  then v else 0], [], [r]))        -- bgz
exeScenario 8 = ((1,0), (1,0,0), \[[v]]       _  -> ([v], [], []))                                      -- jmp
exeScenario 9 = ((1,1), (0,1,1), \[[rs]]     [r] -> ([], [r <@! rs], [r]))                           -- printf
exeScenario _  = error "exeScenario: Non existent scenario"

-- | 'exeKernel' is the Execution (EXE) kernel process
exeKernel :: Signal ScenarioEXE -> Signal [Arg] -> (Signal Arg, Signal Arg)
exeKernel exeCt sigArg = (sigBr, sigPrf)
  where (sigBr, sigPrf, sigReg) = kernel23 exeCt sigArg sigReg' 
        sigReg' = delay [fanoutn 32 0] sigReg

---------------------------------------------------------
-- Decode Detector
---------------------------------------------------------

type ScenarioDEC = ((Prod,[ScenarioIF]), (Prod,[ScenarioEXE]))

-- | 'detectorScenario' is th output function of the detector. It converts a
-- state into an equivalent scenario
decScenario :: StateDEC -> ScenarioDEC
decScenario n = ((1,[ifScenario n]), (1,[exeScenario n]))

-- | 'decSwitchState' is the state transition function of the detector
decSwitchState :: StateDEC -> [Op] -> StateDEC
decSwitchState _ ["nop"]    = 0
decSwitchState _ ["mov"]    = 1
decSwitchState _ ["movi"]   = 2
decSwitchState _ ["add"]    = 3
decSwitchState _ ["sub"]    = 4
decSwitchState _ ["mul"]    = 5
decSwitchState _ ["bez"]    = 6
decSwitchState _ ["bgz"]    = 7
decSwitchState _ ["jmp"]    = 8
decSwitchState _ ["printf"] = 9
decSwitchState _ _ = error "decSwitchState: Input not recognized"

-- | 'decodeDetector' is the detector of ProSyDe
decDetector :: Signal Op -> (Signal ScenarioIF, Signal ScenarioEXE)
decDetector = detector12 1 decSwitchState decScenario 0


---------------------------------------------------------
-- RISC model: process network
---------------------------------------------------------

-- riscSADF :: Signal Arg
riscSADF = sigPrintf
  where (sigBr, sigPrintf) = exeKernel exeCt sigArg
        (sigOp, sigArg) = ifKernel ifCt' sigBr
        (ifCt, exeCt) = decDetector sigOp
        ifCt' = delay [ifScenario 0] ifCt

riscTest = (sigOp,sigArg)
  where (sigOp, sigArg) = ifKernel ifCt' (signal [-3,-3,-6])
        (ifCt, exeCt) = decDetector sigOp
        ifCt' = delay [ifScenario 0] ifCt


---------------------------------------------------------
-- TEST
---------------------------------------------------------

-- | test with
--
-- > takeS 10 riscSADF
program = vector
  [ ("movi", [1,100])
  , ("movi", [2,20])
  , ("printf", [1])
  , ("sub", [1,2])
  , ("printf", [1])
  , ("bgz",  [1,-3])
  , ("jmp", [-7])
  ]
