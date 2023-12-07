module Automaton where 

import Data.Map (Map, foldrWithKey, findWithDefault)
import Data.Map qualified as Map
import Data.Set (Set, insert, empty)
import Data.Set qualified as Set

-- { GENERAL AUTOMATON TYPES }

type State = String
-- UUID attached to all states in an NFA when combining NFAs
-- to ensure there is no name collision
type UUID = Int 
  
data Automaton transitionT acceptingT = Aut
  { initial :: State,
    transition :: transitionT,
    accepting :: acceptingT
  }

-- | Key is the current state and a char for transition
type Transition v = Map (State, Char) v

class MutableTrans t where
  -- | Count number of transitions
  countTransitions :: t -> Int
  -- | Insert a transition
  insertTransition :: t -> (State, Char, State) -> t
  -- | Union 2 transitions
  unionTransitions :: t -> t -> t

instance
  (Show transitionT, Show acceptingT) =>
  Show (Automaton transitionT acceptingT)
  where
  show Aut {initial, transition, accepting} =
    "{ initial"
      ++ show initial
      ++ " ,\n"
      ++ "   transitions: "
      ++ show transition
      ++ " ,\n"
      ++ "   accepting:"
      ++ show accepting
      ++ "\n }"

-- | Get all the characters that have a transition 
getAlphabet :: Ord a => Automaton (Transition a) b -> Set Char
getAlphabet nfa@Aut{transition} = 
  foldrWithKey 
  (\(_, char) _ -> Data.Set.insert char) Data.Set.empty transition

-- | Make a transition, along with an option for a default state if no 
-- such transition exists
makeTransition :: Transition v -> v -> Char -> State -> v
makeTransition trans def char st =
  Map.findWithDefault def (st, char) trans