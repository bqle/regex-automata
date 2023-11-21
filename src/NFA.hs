module NFA where 

import qualified Data.List as List
import Data.Set.Monad (Set, 
  fromList
  , empty) -- Note, we are using Data.Set.Monad instead of regular Set
import Data.List (nub)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC
import Data.Foldable          (foldlM)
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, lookup)

type Transition s a = Map (s, a) [s]

makeTransition :: (Ord s, Ord a) => Transition s a -> a -> s -> [s]
makeTransition t a s  = fromMaybe [] (Data.Map.lookup (s, a) t)

-- | Finite automaton with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
data Automaton s a = Automaton
  { initial    :: [s]               -- ^ Initial State
  , transition :: Transition s a  -- ^ Change state with a context.
  , accepting  :: s -> Bool       -- ^ Accepting subset as a predicate.
  }

type NFA = Automaton String Char

exampleNFA :: NFA
exampleNFA = Automaton {
  initial = ["0"], 
  transition = Data.Map.fromList [(("0", 'a'), ["1", "2", "3"])],
  accepting = const True
}

-- Run an automata & get the final states
run :: forall s a. (Ord s, Ord a) => Automaton s a -> [a] -> [s]
run Automaton {initial, transition, accepting} = 
  foldl (findNextState transition) initial 
  where 
    findNextState :: Transition s a -> [s] -> a -> [s]
    findNextState transition states a = 
      concatMap (makeTransition transition a) states


-- >>> run exampleNFA "a"
-- ["1","2","3"]

type DFA = Automaton String Char

exampleDFA :: DFA
exampleDFA = Automaton {
  initial = ["0"], 
  transition = Data.Map.fromList [(("0", 'a'), ["0"])],
  accepting = const True
}

-- >>> run exampleDFA "a"
-- ["0"]

