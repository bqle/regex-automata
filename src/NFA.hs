module NFA where

-- Note, we are using Data.Set.Monad instead of regular Set

import Control.Monad.Identity (Identity (..))
import Crypto.Hash (hashWith)
import Data.Foldable (foldlM)
import Data.List (nub)
import Data.List qualified as List
import Data.Map (Map, empty, foldrWithKey, fromList, insert, lookup, union)
import Data.Maybe (fromMaybe)
import Data.Set.Monad
  ( Set,
    empty,
    fromList,
  )
import RandomString (hashString, randomUUID)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

-- { GENERAL AUTOMATON TYPES }

data Automaton transitionT acceptingT = Aut
  { uuid :: Int,
    initial :: String,
    transition :: transitionT,
    accepting :: acceptingT
  }

-- | (current state, new char) -> new states
type Transition v = Map (String, Char) v

makeTransition :: Transition v -> v -> Char -> String -> v
makeTransition trans def char st =
  fromMaybe def (Data.Map.lookup (st, char) trans)

-- { NFA SPECIALIZATIONS }
type NFATransition = Transition [String]

-- | Finite NFA with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
--  Assumptions: 1 initial state, 1 accepting state
type NFA = Automaton NFATransition String

instance
  (Show transitionT, Show acceptingT) =>
  Show (Automaton transitionT acceptingT)
  where
  show Aut {uuid, initial, transition, accepting} =
    "{  uuid: "
      ++ show uuid
      ++ " ,\n"
      ++ "   initial"
      ++ show initial
      ++ " ,\n"
      ++ "   transitions: "
      ++ show transition
      ++ " ,\n"
      ++ "   accepting:"
      ++ show accepting
      ++ "\n }"

-- | Represents the epsilon transitions in NFAs
epsilon = '\0'

exampleNFA :: NFA
exampleNFA =
  Aut
    { uuid = 0,
      initial = "0",
      transition = Data.Map.fromList [(("0", 'a'), ["0"])],
      accepting = "0"
    }

neverAcceptNFA :: NFA
neverAcceptNFA =
  Aut
    { uuid = 0,
      initial = "0",
      transition = Data.Map.empty,
      accepting = "1"
    }

-- | Find all transitions after taking the char
findNextStates :: NFATransition -> [String] -> Char -> [String]
findNextStates transition starting a =
  nub $
    exploreEpsilons
      transition
      (nub $ concatMap (makeTransition transition [] a) starting)

-- | Expand the states to include all reachable states thru epsilon transitions
exploreEpsilons :: NFATransition -> [String] -> [String]
exploreEpsilons transition states =
  let immediateFrontier =
        nub $
          concatMap
            (makeTransition transition [] epsilon)
            states
   in case immediateFrontier of
        [] -> states
        _ -> exploreEpsilons transition immediateFrontier ++ states

-- | Run an NFA & get the final states
run :: NFA -> [Char] -> [String]
run Aut {initial, transition, accepting} =
  foldl (findNextStates transition) (exploreEpsilons transition [initial])

accept :: NFA -> [Char] -> Bool
accept nfa@Aut {accepting} s =
  (accepting `elem`) $ run nfa s

-- | Attach the uuid to every state, update transitions accordingly
attachUUID :: NFA -> NFA
attachUUID Aut {uuid, initial, transition, accepting} =
  Aut uuid newInitial newTransitions newAccepting
  where
    hashWithUUID str = hashString (str ++ show uuid)
    newInitial = hashWithUUID initial
    newAccepting = hashWithUUID accepting
    newTransitions =
      foldrWithKey
        ( \(state, char) val ->
            Data.Map.insert
              (hashWithUUID state, char)
              (List.map hashWithUUID val)
        )
        Data.Map.empty
        transition

-- | Union two transitions
unionTransitions :: NFATransition -> NFATransition -> NFATransition
unionTransitions t1 t2 =
  foldrWithKey
    ( \(state, char) dest acc ->
        Data.Map.insert
          (state, char)
          ( nub (fromMaybe [] (Data.Map.lookup (state, char) acc) ++ dest)
          )
          acc
    )
    t2
    t1

-- | Create fully bipartite graph from two lists of vertices
--   We need original transitions because accepting states may already have
--   e-transitions
bipartiteTransitions :: NFATransition -> [String] -> [String] -> NFATransition
bipartiteTransitions transOrig s1 s2 =
  foldl
    ( \acc acceptS ->
        foldl
          ( \acc initS ->
              Data.Map.insert
                (acceptS, epsilon)
                ( nub
                    ( initS
                        : fromMaybe [] (Data.Map.lookup (acceptS, epsilon) acc)
                    )
                )
                acc
          )
          acc
          s2
    )
    Data.Map.empty
    s1

-- { CORE NFA Operations }

-- | Unit NFA accepts a set of chars
alphabet :: [Char] -> NFA
alphabet ls =
  let initSt = "i"
      accSt = "e"
      transitions =
        foldl
          (\acc v -> Data.Map.insert (initSt, v) [accSt] acc)
          Data.Map.empty
          ls
   in Aut 0 initSt transitions accSt

-- | Append 2 NFA (ab)
append :: NFA -> NFA -> NFA
append nfa1 nfa2 =
  Aut 0 init1 newTransitions accept2
  where
    Aut uuid1 init1 trans1 accept1 = attachUUID nfa1
    Aut uuid2 init2 trans2 accept2 = attachUUID nfa2
    newConnections = bipartiteTransitions trans1 [accept1] [init2]
    newTransitions =
      unionTransitions newConnections (unionTransitions trans1 trans2)

-- | Alternate 2 NFA (a + b) (basically OR)
alternate :: NFA -> NFA -> NFA
alternate nfa1 nfa2 =
  let newInitSt = "i"
      newAccSt = "e"
      newConnections =
        unionTransitions
          (bipartiteTransitions Data.Map.empty [newInitSt] [init1, init2])
          ( bipartiteTransitions
              (Data.Map.union trans1 trans2)
              [accept1, accept2]
              [newAccSt]
          )
      newTransitions =
        unionTransitions
          newConnections
          (unionTransitions trans1 trans2)
   in Aut 0 newInitSt newTransitions newAccSt
  where
    Aut uuid1 init1 trans1 accept1 = attachUUID nfa1
    Aut uuid2 init2 trans2 accept2 = attachUUID nfa2

-- | kleene-star (a*)
kleene :: NFA -> NFA
kleene nfa =
  let newInitSt = "i"
      newAccSt = "e"
      Aut _ init trans accept = attachUUID nfa
      newTransitions =
        unionTransitions
          (bipartiteTransitions Data.Map.empty [newInitSt] [newAccSt, init])
          (bipartiteTransitions trans [accept] [newAccSt, init])
   in Aut 0 newInitSt (unionTransitions newTransitions trans) newAccSt
