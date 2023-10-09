module Turing where

import Data.Foldable (toList)
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), ViewL (..), ViewR(..))
import System.Environment
import System.Exit

data Action = Halt | Start | ScanRight | ScanLeft | ArgA | ArgB | Subtract | Accept
             deriving (Eq, Show)

type Symbol = Char

-- | Tape head movement direction.
data Direction = MoveLeft | MoveRight
                 deriving (Show, Eq)

data Quintuple = Quintuple {
--transition function: (current state, current symbol)
--mapped to: (new state, new symbol, direction)
--current symbol is analogous to the tape head
--if the symbol is "Just x", x is the current symbol under the tape head
--if symbol is "Nothing", the cell has not been initialized
  transition :: Action -> Maybe Symbol -> (Action, Symbol, Direction),
  startState :: Action,
  acceptState :: Action,
  rejectState :: Action
}

data Tape = Tape {
  leftSyms     :: Seq.Seq Symbol,    -- symbols to the left of tape head
  currentSym   :: Maybe Symbol,      -- symbol under the tape head
  rightSyms    :: Seq.Seq Symbol     -- symbols to the right of tape head
}

data Machine = Machine {
  currentState :: Action,
  tape :: Tape
}

instance Show Tape where
     show (Tape l c r) = toList l ++ [maybe ' ' id c] ++ toList r

instance Show Machine where
  show (Machine s t) =
    show t ++ "\n" ++ replicate (Seq.length $ leftSyms t) ' ' ++ "|           CURRENT STATE: " ++ show s
  showList = showString . L.intercalate "\n\n" . map show

-- | Replace symbol under tape head with new symbol, then move tape head.
updateTape :: Tape -> Symbol -> Direction -> Tape
updateTape (Tape lSyms _ rSyms) newSym MoveLeft =
  case Seq.viewr lSyms of Seq.EmptyR -> Tape Seq.Empty Nothing right
                          lInit :> lLast -> Tape lInit (Just lLast) right
  where right = newSym <| rSyms
updateTape (Tape lSyms _ rSyms) newSym MoveRight =
  case Seq.viewl rSyms of Seq.EmptyL -> Tape left Nothing Seq.Empty
                          rHead :< rTail -> Tape left (Just rHead) rTail
  where left = lSyms |> newSym

-- | Execute one transition step for given machine and config.
updateMachine :: Quintuple -> Machine -> Machine
updateMachine m (Machine state tape) =
  let (state', newSym, dir) = transition m state (currentSym tape)
  in Machine state' $ updateTape tape newSym dir

-- | Initialise tape with input word.
initTape :: [Symbol] -> Tape
initTape [] = Tape Seq.empty Nothing Seq.empty
initTape (x:xs) = Tape Seq.empty (Just x) (Seq.fromList xs)

-- | Initialise machine config with input word.
initMachine :: Quintuple -> [Symbol] -> Machine
initMachine m input = Machine (startState m) (initTape input)

-- | Return true if the machine is in a final state.
machineCfg :: Quintuple -> Machine -> Bool
machineCfg m (Machine {currentState = c}) =
  c == acceptState m ||
  c == rejectState m

-- | Return sequence of machine configs for given input word until final state.
runMachine :: Quintuple -> [Symbol] -> [Machine]
runMachine m =
  send (machineCfg m) . iterate (updateMachine m) . initMachine m
  where
    send p xs = let (prefix, rest) = break p xs in prefix ++ [head rest]

instruction :: Action -> Maybe Symbol -> (Action, Symbol, Direction)
instruction Start (Just '_') = (Start, '_', MoveRight)
instruction Start (Just '1') = (ScanRight, '.', MoveRight)
instruction ScanRight (Just '1') = (ScanRight, '1', MoveRight)
instruction ScanRight (Just '0') = (ArgA, '0', MoveRight)
instruction ArgA (Just '.') = (ArgA, '.', MoveRight)
instruction ArgA (Just '_') = (Accept, '_', MoveRight)
instruction ArgA (Just '1') = (ScanLeft, '.', MoveLeft)
instruction ScanLeft (Just '.') = (ScanLeft, '.', MoveLeft)
instruction ScanLeft (Just '0') = (ArgB, '0', MoveLeft)
instruction ArgB (Just '1') = (ArgB, '1', MoveLeft)
instruction ArgB (Just '.') = (Start, '.', MoveRight)
instruction Start (Just '0') = (Subtract, '0', MoveRight)
instruction Subtract (Just '.') = (Subtract, '_', MoveRight)
instruction Subtract (Just '1') = (Halt, '1', MoveRight)

-- | A Turing machine performing subtraction
turingMachine :: Quintuple
turingMachine = Quintuple { 
            transition = instruction,
            startState = Start,
            acceptState = Accept,
            rejectState = Halt }

main x y = do
           let inp = "_" ++ x ++ "0" ++ y ++ "_"
           print $ runMachine turingMachine inp
