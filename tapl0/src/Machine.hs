module Machine where

import Syntax
import Data.Map.Lazy as M
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Either.Utils

data HeapValue
  = HeapInst { getSeq :: InstSequence }
  | Bottom -- Just for generality, if other things can live on heap later

instance Show HeapValue where
  show (HeapInst is) = show is
  show Bottom = "ο"

type RegisterFile = M.Map Register Operand
type Heap = M.Map Label HeapValue
type MachineState = (Heap, RegisterFile, InstSequence)

showMap :: (Show k, Show v) => M.Map k v -> Bool -> IO ()
showMap map nl =
  sequence_ $ Prelude.map (\(r, v) -> do
                     putStr $ id $ show r
                     putStr ": "
                     if (nl) then putStr "\n" else putStr ""
                     print v
                     ) (M.toList map)

-- TODO add fields for more details
data MachineError =
    InvalidLabel
  | NonNumericOperands
  | InvalidJumpCondition
  deriving (Show)

type MachineMonad = Either MachineError

initialState :: Program -> MachineState
initialState p =
  let heap = M.map HeapInst p
      regs = initRegisters 32 -- arbitrary number of registers right now
      entry = Jump (OpLabel $ Label "main")
  in (heap, regs, entry)

interpret :: MachineState -> MachineState
interpret state =
  case step state of
    Left e -> state
    Right nextState ->
      if (isHalted nextState)
      then nextState
      else interpret nextState
-- TODO actually handle error (use catchError?)

isHalted :: MachineState -> Bool
isHalted (_, _, Halt) = True
isHalted _ = False

step :: MachineState -> MachineMonad MachineState
step (heap, regFile, insts) = case insts of
  Halt -> return (heap, regFile, Halt)
  Jump v -> do
    is <- targetInsts v heap regFile
    return (heap, regFile, is)
  Seq i is -> case i of
    Move destReg value -> do
      let regFile1 = M.insert destReg (rhat regFile value) regFile
      return (heap, regFile1, is)
    Add destReg srcReg value -> do
      sum <- add (regFile ! srcReg) (rhat regFile value)
      let regFile1 = M.insert destReg sum regFile
      return (heap, regFile1, is)
    IfZero cond destLabel ->
      case regFile ! cond of
        (OpLit 0) -> do
          is1 <- targetInsts destLabel heap regFile
          return (heap, regFile, is1)
        (OpLit _) -> do
          return (heap, regFile, is)
        _ -> throwError InvalidJumpCondition

add :: Operand -> Operand -> MachineMonad Operand
add (OpLit n1) (OpLit n2) = return $ OpLit $ n1 + n2
add _ _ = throwError NonNumericOperands

targetInsts :: Operand -> Heap -> RegisterFile -> MachineMonad InstSequence
targetInsts o h r =
  case (rhat r o) of
    ol@(OpLabel l) -> do
      hi <- maybeToEither InvalidLabel $ (M.lookup l h)
      return $ getSeq hi
    _ -> throwError InvalidLabel

rhat :: RegisterFile -> Operand -> Operand
rhat regFile (OpRegister r) = regFile ! r
rhat _ o = o

initRegisters :: Int -> RegisterFile
initRegisters k = M.fromList (zip (Prelude.map Register [1..k]) (repeat (OpLit 0)))

showState :: MachineState -> IO ()
showState (heap1, regs1, insts) = do
  putStrLn "Heap:\n====="
  showMap heap1 True
  putStrLn ""
  putStrLn "Registers:\n=========="
  showMap regs1 False
  putStrLn ""
