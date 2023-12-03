module NFA (
  NFA,
  alphabet, 
  append,
  kleene,
  run,
  accept,
  alternate,
  findAcceptingString,
  neverAcceptNFA,
  makeTransition,
  exploreEpsilons,
  findNextStates,
  getAlphabet,
  countTransitions,
  insertConnection,
  bipartiteTransitions,
  Transition,
  State,
  Automaton (..),
) where

import Data.List qualified as List
import Data.Map (Map, empty, foldrWithKey, fromList, insert, lookup, union,
  findWithDefault)
import Data.Map qualified as Map
import Data.Set (Set, insert, empty, toList, member, insert, fromList, fold, union)
import Data.Set qualified as Set
import Data.Maybe (isJust)
import RandomString (hashString)

-- { GENERAL AUTOMATON TYPES }

type State = String
type UUID = Int

data Automaton transitionT acceptingT = Aut
  { initial :: State,
    transition :: transitionT,
    accepting :: acceptingT
  }

-- | (current state, new char) -> new states
type Transition v = Map (State, Char) v

-- | Get all the characters that have a transition 
getAlphabet :: Ord a => Automaton (Transition a) b -> Set Char
getAlphabet nfa@Aut{transition} = 
  foldrWithKey 
  (\(_, char) _ -> Data.Set.insert char) Data.Set.empty transition

makeTransition :: Transition v -> v -> Char -> State -> v
makeTransition trans def char st =
  Map.findWithDefault def (st, char) trans

-- { NFA SPECIALIZATIONS }
type NFATransition = Transition (Set State)

-- | Finite NFA with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
--  Assumptions: 1 initial state, 1 accepting state
type NFA = Automaton NFATransition State

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

-- | Represents the epsilon transitions in NFAs
epsilon = '\0'

exampleNFA :: NFA
exampleNFA =
  Aut
    { initial = "0",
      transition = Map.fromList [(("0", 'a'), Data.Set.fromList ["0"])],
      accepting = "0"
    }

neverAcceptNFA :: NFA
neverAcceptNFA =
  Aut
    { initial = "0",
      transition = Map.empty,
      accepting = "1"
    }

-- | Find all transitions after taking the char
findNextStates :: NFATransition -> Set State -> Char -> Set State
findNextStates transition starting a =
    exploreEpsilons
      transition
      (Set.fold (\v acc -> Set.union acc 
        (makeTransition transition Data.Set.empty a v)) Data.Set.empty starting) 

-- | Expand the states to include all reachable states thru epsilon transitions
exploreEpsilons :: NFATransition -> Set State -> Set State
exploreEpsilons transition states =
  let 
    immediateFrontier = Set.fold (\v acc -> Set.union acc 
        (makeTransition transition Data.Set.empty epsilon v)
      ) Set.empty states 
    in if immediateFrontier == Data.Set.empty then states
      else Data.Set.union (exploreEpsilons transition immediateFrontier) states

-- | Run an NFA & get the final states
run :: NFA -> [Char] -> Set State
run Aut {initial, transition, accepting} =
  List.foldl (findNextStates transition) (exploreEpsilons transition 
    (Set.singleton initial))

accept :: NFA -> [Char] -> Bool
accept nfa@Aut {accepting} s =
  (accepting `elem`) $ run nfa s

-- | Attach the uuid to every state, update transitions accordingly
attachUUID :: NFA -> Int -> NFA
attachUUID Aut {initial, transition, accepting} uuid =
  Aut newInitial newTransitions newAccepting
  where
    hashWithUUID str = hashString (str ++ show uuid)
    newInitial = hashWithUUID initial
    newAccepting = hashWithUUID accepting
    newTransitions =
      foldrWithKey
        ( \(state, char) val ->
            Map.insert
              (hashWithUUID state, char)
              (Set.map hashWithUUID val)
        )
        Map.empty
        transition

-- | Union two transitions
unionTransitions :: NFATransition -> NFATransition -> NFATransition
unionTransitions t1 t2 =
  foldrWithKey
    ( \(state, char) dest acc ->
        Map.insert
          (state, char)
          (Set.union dest (
            Map.findWithDefault Set.empty (state,char) acc
          ))
          acc
    )
    t2
    t1

-- | Insert new connection into NFA 
insertConnection :: NFATransition -> (State, Char, State) -> NFATransition
insertConnection trans (u, c, v) = 
  case Map.lookup (u, c) trans of
    Nothing -> Map.insert (u, c) (Set.singleton v) trans
    Just arr -> Map.insert (u, c) 
      (Set.union (if v `elem` arr then Set.empty else Set.singleton v) arr)
      trans

-- | Create fully bipartite graph from two lists of vertices
--   We need original transitions because accepting states may already have
--   e-transitions
bipartiteTransitions :: NFATransition -> Set State -> Set State -> NFATransition
bipartiteTransitions transOrig s1 s2 =
  foldl ( \acc uS -> 
      Map.insert (uS, epsilon) 
      (Set.union s2
        (makeTransition acc Set.empty epsilon uS )
      )
      acc
    )
    Map.empty
    s1

-- | Count number of transitions from an NFATransition
countTransitions :: NFATransition -> Int
countTransitions =
  Map.foldrWithKey (\(k, c) vs acc -> acc + length vs) 0 

-- { CORE NFA Operations }

-- | Unit NFA accepts a set of chars
alphabet :: [Char] -> NFA
alphabet ls =
  let initSt = "i"
      accSt = "e"
      transitions =
        foldl
          (\acc v -> Map.insert (initSt, v) (Set.singleton accSt) acc)
          Map.empty
          ls
   in Aut initSt transitions accSt

-- | Append 2 NFA (ab)
append :: (NFA, UUID) -> (NFA, UUID) -> NFA
append (nfa1, uuid1) (nfa2, uuid2) =
  Aut init1 newTransitions accept2
  where
    Aut init1 trans1 accept1 = attachUUID nfa1 uuid1
    Aut init2 trans2 accept2 = attachUUID nfa2 uuid2
    newConnections = 
      bipartiteTransitions trans1 (Set.singleton accept1) (Set.singleton init2)
    newTransitions =
      unionTransitions newConnections (unionTransitions trans1 trans2)

-- | Alternate 2 NFA (a + b) (basically OR)
alternate :: (NFA, UUID) -> (NFA, UUID) -> NFA
alternate (nfa1, uuid1) (nfa2, uuid2) =
  let newInitSt = "i"
      newAccSt = "e"
      newConnections =
        unionTransitions
          (bipartiteTransitions Map.empty (Set.singleton newInitSt) 
            (Set.fromList [init1, init2]))
          ( bipartiteTransitions
              (Map.union trans1 trans2)
              (Set.fromList [accept1, accept2])
              (Set.singleton newAccSt)
          )
      newTransitions =
        unionTransitions
          newConnections
          (unionTransitions trans1 trans2)
   in Aut newInitSt newTransitions newAccSt
  where
    Aut init1 trans1 accept1 = attachUUID nfa1 uuid1
    Aut init2 trans2 accept2 = attachUUID nfa2 uuid2

-- | kleene-star (a*)
kleene :: (NFA, UUID) -> NFA
kleene (nfa, uuid) =
  let newInitSt = "i"
      newAccSt = "e"
      Aut init trans accept = attachUUID nfa uuid
      newTransitions =
        unionTransitions
          (bipartiteTransitions Map.empty (Set.singleton newInitSt) 
            (Set.fromList [newAccSt, init]))
          (bipartiteTransitions trans (Set.singleton accept) 
            (Set.fromList [newAccSt, init]))
   in Aut newInitSt (unionTransitions newTransitions trans) newAccSt

-- TESTING -- 
-- | Helper for find accpting string given nfa, starting state, and visited 
-- states
findAcceptingStringAux :: NFA -> State -> Set State -> Maybe String
findAcceptingStringAux nfa@Aut{initial, transition, accepting} start 
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
          else case Map.lookup (start, char) transition of
            Nothing -> Nothing 
            Just nexts -> 
              do 
                v <- foldl 
                  (\acc next -> if isJust acc then acc 
                    else findAcceptingStringAux nfa next (Set.insert start visited)) 
                      Nothing nexts
                return (char : v)
        ) Nothing alphabet 

-- | Find an example of an accepted string
findAcceptingString :: NFA -> Maybe String
findAcceptingString nfa = 
  findAcceptingStringAux nfa (initial nfa) Set.empty
