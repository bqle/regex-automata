module NFA where

import Control.Monad.Identity (Identity (..))
import Crypto.Hash (hashWith)
import Data.Foldable (foldlM)
import Data.List (nub)
import Data.List qualified as List
import Data.Map (Map, empty, foldrWithKey, fromList, insert, lookup, union,
  findWithDefault)
import Data.Set (Set, insert, empty, toList, member, insert, fromList, fold, union)
import Data.Set qualified as Set
import Data.Maybe (fromMaybe, isJust)
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
  Data.Map.findWithDefault def (st, char) trans
--   fromMaybe def (Data.Map.lookup (st, char) trans)

-- { NFA SPECIALIZATIONS }
type NFATransition = Transition (Set String)

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
      transition = Data.Map.fromList [(("0", 'a'), Data.Set.fromList ["0"])],
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
findNextStates :: NFATransition -> Set String -> Char -> Set String
findNextStates transition starting a =
    exploreEpsilons
      transition
      (Set.fold (\v acc -> Set.union acc 
        (makeTransition transition Data.Set.empty a v)) Data.Set.empty starting) 

-- | Expand the states to include all reachable states thru epsilon transitions
exploreEpsilons :: NFATransition -> Set String -> Set String
exploreEpsilons transition states =
  let 
    immediateFrontier = Set.fold (\v acc -> Set.union acc 
        (makeTransition transition Data.Set.empty epsilon v)
      ) Set.empty states 
        -- nub $
        --   concatMap
        --     (makeTransition transition [] epsilon)
        --     states
    in if immediateFrontier == Data.Set.empty then states
      else Data.Set.union (exploreEpsilons transition immediateFrontier) states
    -- case immediateFrontier of
    --     Data.Set.empty -> states
    --     _ -> Data.Set.union (exploreEpsilons transition immediateFrontier) states

-- | Run an NFA & get the final states
run :: NFA -> [Char] -> Set String
run Aut {initial, transition, accepting} =
  List.foldl (findNextStates transition) (exploreEpsilons transition (Set.singleton initial))

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
              (Set.map hashWithUUID val)
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
          (Set.union dest (
            Data.Map.findWithDefault Set.empty (state,char) acc
          ))
          acc
    )
    t2
    t1

-- | Insert new connection into NFA 
insertConnection :: NFATransition -> (String, Char, String) -> NFATransition
insertConnection trans (u, c, v) = 
  case Data.Map.lookup (u, c) trans of
    Nothing -> Data.Map.insert (u, c) (Set.singleton v) trans
    Just arr -> Data.Map.insert (u, c) 
      (Set.union (if v `elem` arr then Set.empty else Set.singleton v) arr)
      trans

-- | Create fully bipartite graph from two lists of vertices
--   We need original transitions because accepting states may already have
--   e-transitions
bipartiteTransitions :: NFATransition -> Set String -> Set String -> NFATransition
bipartiteTransitions transOrig s1 s2 =
  foldl ( \acc uS -> 
      Data.Map.insert (uS, epsilon) 
      (Set.union s2
        (makeTransition acc Set.empty epsilon uS )
        -- (Data.Map.findWithDefault (uS, epsilon))
      )
    --   ( nub 
    --     (fromMaybe [] (Data.Map.lookup (uS, epsilon) acc) ++ newS2)
    --   )
      acc
    )
    Data.Map.empty
    s1

-- | Get all the characters that have a transition in DFA
getAlphabet :: NFA -> [Char]
getAlphabet nfa@Aut{transition} = 
  Data.Set.toList $ foldrWithKey (\(_, char) _ -> Data.Set.insert char) Data.Set.empty transition

-- | Count number of transitions from an NFATransition
countTransitions :: NFATransition -> Int
countTransitions =
  Data.Map.foldrWithKey (\(k, c) vs acc -> acc + length vs) 0 

-- { CORE NFA Operations }

-- | Unit NFA accepts a set of chars
alphabet :: [Char] -> NFA
alphabet ls =
  let initSt = "i"
      accSt = "e"
      transitions =
        foldl
          (\acc v -> Data.Map.insert (initSt, v) (Set.singleton accSt) acc)
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
    newConnections = 
      bipartiteTransitions trans1 (Set.singleton accept1) (Set.singleton init2)
    newTransitions =
      unionTransitions newConnections (unionTransitions trans1 trans2)

-- | Alternate 2 NFA (a + b) (basically OR)
alternate :: NFA -> NFA -> NFA
alternate nfa1 nfa2 =
  let newInitSt = "i"
      newAccSt = "e"
      newConnections =
        unionTransitions
          (bipartiteTransitions Data.Map.empty (Set.singleton newInitSt) 
            (Set.fromList [init1, init2]))
          ( bipartiteTransitions
              (Data.Map.union trans1 trans2)
              (Set.fromList [accept1, accept2])
              (Set.singleton newAccSt)
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
          (bipartiteTransitions Data.Map.empty (Set.singleton newInitSt) 
            (Set.fromList [newAccSt, init]))
          (bipartiteTransitions trans (Set.singleton accept) 
            (Set.fromList [newAccSt, init]))
   in Aut 0 newInitSt (unionTransitions newTransitions trans) newAccSt

-- TESTING -- 
-- | Helper for find accpting string given nfa, starting state, and visited 
-- states
findAcceptingStringH :: NFA -> String -> [String] -> Maybe String
findAcceptingStringH nfa@Aut{uuid, initial, transition, accepting} start 
  visited 
  | start == accepting = Just "" 
  | otherwise = 
    if start `elem` visited then Nothing
    else let 
      alphabet = getAlphabet nfa 
      in 
        foldr 
        (\char acc -> 
          if isJust acc then acc
          else case Data.Map.lookup (start, char) transition of
            Nothing -> Nothing 
            Just nexts -> 
              do 
                v <- foldl 
                  (\acc next -> if isJust acc then acc 
                    else findAcceptingStringH nfa next (start : visited)) Nothing nexts
                return (char : v)
        ) Nothing alphabet 

-- | Find an example of an accepted string
findAcceptingString :: NFA -> Maybe String
findAcceptingString nfa = 
  findAcceptingStringH nfa (initial nfa) []
