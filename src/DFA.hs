module DFA where

import NFA
import Data.Set (Set, insert, empty, toList, member, insert, fromList)
import Data.Map (Map, fromList, foldrWithKey, empty, insert)
import Data.List (sort, foldl)

-- { DFA SPECIALIZATION }
type DFATransition = Transition String

type DFA = Automaton DFATransition [String]

-- | A rejecting state for all DFAs. Will be forever dead once reach this state
rejectingSt :: String
rejectingSt = "r"

-- | Run a DFA & get the final state
run :: DFA -> [Char] -> String
run Aut {uuid, initial, transition, accepting} = 
  Prelude.foldl (flip (makeTransition transition rejectingSt)) initial

accept :: DFA -> [Char] -> Bool
accept dfa@Aut {accepting} s = DFA.run dfa s `elem` accepting

-- | Get all the characters that have a transition in DFA
getAlphabet :: NFA -> [Char]
getAlphabet nfa@Aut{transition} = 
  Data.Set.toList $ foldrWithKey (\(_, char) _ -> Data.Set.insert char) Data.Set.empty transition


-- | Convert NFA to DFA
convert :: NFA -> DFA
convert nfa@Aut{uuid, initial, transition, accepting} = 
  Aut 0 initialDfa newTransition acceptedSt
  where 
    alphabet = getAlphabet nfa
    flatten :: [String] -> String
    flatten nfaStates = show (sort nfaStates)
    exploreAllTransitions :: (DFATransition, Set String, [String]) 
      -> [String] -> (DFATransition, Set String, [String])
    exploreAllTransitions acc currentStates =
      let current = flatten currentStates in 
      foldl (\acc@(trans, visited, acceptedSt) char ->
        let 
          nextStates = findNextStates transition currentStates char
          next = flatten nextStates 
          newTrans = Data.Map.insert (current, char) next trans in
        if member next visited 
          then (newTrans, visited, acceptedSt)
          else let
            newTrans = Data.Map.insert (current, char) next trans
            newVisited = Data.Set.insert next visited 
            newAcceptedSt = [next | accepting `elem` nextStates] ++ acceptedSt
          in
          exploreAllTransitions (newTrans, newVisited, newAcceptedSt) nextStates
        ) acc alphabet
    initialStates = exploreEpsilons transition [initial]
    initialDfa = flatten initialStates
    (newTransition, v, acceptedSt) = exploreAllTransitions (Data.Map.empty, 
      Data.Set.fromList [initialDfa], 
       [initialDfa | accepting `elem` initialStates]) initialStates

  

 