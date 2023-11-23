module DFATest where

import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import DFA 
import NFA (transition, NFA, makeTransition, findNextStates)
import NFATest

alphabetDFA :: DFA
alphabetDFA = convert alphabetNFA

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
    accept kleeneDFA "a" ~?= True,
    accept kleeneDFA "" ~?= True,
    accept kleeneDFA "b" ~?= True,
    accept kleeneDFA "aa" ~?= True,
    accept kleeneDFA "aabb" ~?= True
  ]

appendDFA = convert appendNFA

tAppendDFA = 
  "appendDFA" ~: TestList [
    accept appendDFA "a" ~?= False,
    accept appendDFA "" ~?= False,
    accept appendDFA "ba" ~?= True
  ]

alternateDFA = convert alternateNFA

tAlternateDFA = 
  "alternateDFA" ~: TestList [
    accept alternateDFA "a" ~?= True,
    accept alternateDFA "ab" ~?= True,
    accept alternateDFA "abc" ~?= False,
    accept alternateDFA "" ~?= False
  ]

test_all_dfa = runTestTT $ TestList [tAlphabetDFA, tAlternateDFA, tAppendDFA, tKleeneDFA]

-- >>> test_all_dfa
-- Counts {cases = 16, tried = 16, errors = 0, failures = 0}
