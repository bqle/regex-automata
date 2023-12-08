module DFA 
(
  DFA,
  DFA.run,
  DFA.accept,
  convert,
  isSubset,
) 
where

import NFA 
import Automaton
import Data.Set (Set, insert, empty, toList, member, insert, fromList, singleton)
import Data.Set qualified as Set
import Data.Map (Map, fromList, foldrWithKey, empty, insert)
import Data.Map qualified as Map
import Data.List (sort, foldl)
import RandomString (hashString)

-- { DFA SPECIALIZATION }
type DFATransition = Transition State

type DFA = Automaton DFATransition (Set State)

instance MutableTrans DFATransition where
  unionTransitions :: DFATransition -> DFATransition -> DFATransition
  unionTransitions t1 = 
    foldrWithKey ( \(state, char) dest acc ->
          case Map.lookup (state, char) t1 of
            Nothing -> Map.insert (state, char) dest acc
            Just _ -> acc
    ) t1 

  countTransitions :: DFATransition -> Int
  countTransitions = 
    Map.foldrWithKey (\_ _ acc -> acc + 1) 0

  insertTransition :: DFATransition -> (State, Char, State) -> DFATransition
  insertTransition trans (u, c, v) = 
    Map.insert (u, c) v trans

-- | A rejecting state for all DFAs. Will be forever dead once reach this state
rejectingSt :: State
rejectingSt = "r"

-- | Run a DFA & get the final state
run :: DFA -> [Char] -> State
run Aut {initial, transition} = 
  Prelude.foldl (flip (makeTransition transition rejectingSt)) initial

accept :: DFA -> [Char] -> Bool
accept dfa@Aut {accepting} s = DFA.run dfa s `elem` accepting

-- | Convert NFA to DFA
convert :: NFA -> DFA
convert nfa@Aut{initial, transition, accepting} = 
  Aut initialDfa newTransition acceptedSt
  where 
    alphabet :: Set Char
    alphabet = getAlphabet nfa
    -- | Group a set of states into a single state
    flatten :: Set State -> State
    flatten nfaStates = hashString $ show (sort (Set.toList nfaStates))
    -- | Recursive DFA to explore all transitions in the DFA
    exploreAllTransitions :: (DFATransition, Set State, Set State) 
      -> Set String -> (DFATransition, Set State, Set State)
    exploreAllTransitions acc currentStates =
      let current = flatten currentStates in 
      foldl (\acc@(trans, visited, acceptedSt) char ->
        let 
          nextStates = findNextStates transition currentStates char
          next = flatten nextStates 
          newTrans = Map.insert (current, char) next trans in
        if char == '\NUL' then acc
        else if member next visited 
          then (newTrans, visited, acceptedSt)
          else let
            newTrans = Map.insert (current, char) next trans
            newVisited = Data.Set.insert next visited 
            newAcceptedSt = 
              if accepting `elem` nextStates 
                then Data.Set.insert next acceptedSt
                else acceptedSt
          in
          exploreAllTransitions (newTrans, newVisited, newAcceptedSt) nextStates
        ) acc alphabet
    initialState = exploreEpsilons transition (Set.singleton initial)
    initialDfa = flatten initialState
    (newTransition, _, acceptedSt) = exploreAllTransitions (Map.empty, 
      Data.Set.fromList [initialDfa], 
      if accepting `elem` initialState then singleton initialDfa else 
        Set.empty
        )
       initialState

-- | Get set of all reachable states
getReachableStates :: DFA -> Set State 
getReachableStates dfa@Aut {initial} = 
  Set.insert rejectingSt (aux dfa initial Set.empty)
  where 
    alphabet = getAlphabet dfa
    aux dfa@Aut{transition} start visited 
      | start `elem` visited = visited
      | otherwise = 
        foldr (\char acc -> 
          aux dfa (makeTransition transition rejectingSt char start) acc
        ) (Set.insert start visited) alphabet

-- | Create the complement of a DFA ie flipping all states accept/reject
complement :: DFA -> DFA
complement dfa@Aut{initial, transition, accepting} = 
  Aut initial transition (Set.difference allStates accepting)
  where 
    alphabet = getAlphabet dfa
    allStates = getReachableStates dfa 

-- | Intersect two DFAs
intersect :: DFA -> DFA -> DFA
intersect dfa1 dfa2 = 
    Aut (combineSt (initial dfa1) (initial dfa2)) combinedTrans
    combinedAcceptStates
  where 
    alphabet = Set.union (getAlphabet dfa1) (getAlphabet dfa2)
    reachable1 = getReachableStates dfa1
    reachable2 = getReachableStates dfa2
    combineSt st1 st2 = hashString $ st1 ++ "," ++ st2
    combinedTrans = 
      foldr (\(u1, u2, char) acc -> 
        Data.Map.insert (combineSt u1 u2, char) 
          (combineSt 
            (makeTransition (transition dfa1) rejectingSt char u1) 
            (makeTransition (transition dfa2) rejectingSt char u2) 
          ) acc
        ) Data.Map.empty [(u1, u2, char) | u1 <- toList reachable1,
                                    u2 <- toList reachable2, 
                                    char <- toList alphabet]
    combinedAcceptStates = foldr (\st acc ->
      Set.union acc (Set.map (combineSt st) (accepting dfa2))
      ) Set.empty (accepting dfa1)

-- | Check if DFA language is empty 
isDFAEmpty :: DFA -> Bool
isDFAEmpty dfa@Aut{accepting} = not $ any (`elem` reachableStates) accepting
  where reachableStates = getReachableStates dfa

-- | Check whether one DFA's language is a subset of another
isSubset :: DFA -> DFA -> Bool
isSubset dfa1 dfa2 = isDFAEmpty $ intersect dfa1 (complement dfa2)