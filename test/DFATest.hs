module DFATest where

import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import DFA (DFA, accept, convert)
import qualified DFA
import NFA (transition, NFA, makeTransition, findNextStates)
import qualified NFA
import NFATest
import qualified NFATest
import Test.QuickCheck as QC

alphabetDFA :: DFA
alphabetDFA = DFA.convert alphabetNFA

tAlphabetDFA = 
  "AlphabetDFA" ~: TestList [
    DFA.accept alphabetDFA "a" ~?= True,
    DFA.accept alphabetDFA "b" ~?= True,
    DFA.accept alphabetDFA "ab" ~?= False,
    DFA.accept alphabetDFA "1" ~?= False
  ]

kleeneDFA :: DFA
kleeneDFA = convert kleeneNFA 

tKleeneDFA = 
  "kleeneNFA" ~: TestList [
    DFA.accept kleeneDFA "a" ~?= True,
    DFA.accept kleeneDFA "" ~?= True,
    DFA.accept kleeneDFA "b" ~?= True,
    DFA.accept kleeneDFA "aa" ~?= True,
    DFA.accept kleeneDFA "aabb" ~?= True
  ]

appendDFA = convert appendNFA

tAppendDFA = 
  "appendDFA" ~: TestList [
    DFA.accept appendDFA "a" ~?= False,
    DFA.accept appendDFA "" ~?= False,
    DFA.accept appendDFA "ba" ~?= True
  ]

alternateDFA = convert alternateNFA

tAlternateDFA = 
  "alternateDFA" ~: TestList [
    DFA.accept alternateDFA "a" ~?= True,
    DFA.accept alternateDFA "ab" ~?= True,
    DFA.accept alternateDFA "abc" ~?= False,
    DFA.accept alternateDFA "" ~?= False
  ]

test_all_dfa = runTestTT $ TestList [tAlphabetDFA, tAlternateDFA, tAppendDFA, tKleeneDFA]

-- >>> test_all_dfa
-- Counts {cases = 16, tried = 16, errors = 0, failures = 0}

-- QUICKCHECK --

-- For any string, DFA accepts iff NFA accepts
prop_DFAiffNFA :: NFA -> String -> Bool
prop_DFAiffNFA nfa str = NFA.accept nfa str == DFA.accept (convert nfa) str

