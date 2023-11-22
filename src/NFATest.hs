import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import qualified NFA
import NFA

lowercaseChars = ['a'..'b']

alphabetNFA :: NFA 
alphabetNFA = alphabet lowercaseChars

tAlphabetNFA = 
  "AlphabetNFA" ~: TestList [
    accept alphabetNFA "a" ~?= True,
    accept alphabetNFA "b" ~?= True,
    accept alphabetNFA "ab" ~?= False,
    accept alphabetNFA "1" ~?= False
  ]

kleeneNFA :: NFA
kleeneNFA = kleene alphabetNFA

tKleeneNFA = 
  "kleeneNFA" ~: TestList [
    accept kleeneNFA "a" ~?= True,
    accept kleeneNFA "" ~?= True,
    accept kleeneNFA "b" ~?= True,
    accept kleeneNFA "aa" ~?= True,
    accept kleeneNFA "aabb" ~?= True
  ]

appendNFA :: NFA 
appendNFA = append alphabetNFA alphabetNFA{uuid=1}

tAppendNFA = 
  "appendNFA" ~: TestList [
    accept appendNFA "a" ~?= False,
    accept appendNFA "" ~?= False,
    accept appendNFA "ba" ~?= True
  ]

alternateNFA :: NFA
alternateNFA = alternate alphabetNFA{uuid=2} appendNFA{uuid=3}

tAlternateNFA = 
  "alternateNFA" ~: TestList [
    accept alternateNFA "a" ~?= True,
    accept alternateNFA "ab" ~?= True,
    accept alternateNFA "abc" ~?= False,
    accept alternateNFA "" ~?= False
  ]

test_all :: IO Counts
test_all = runTestTT $ TestList [tAlternateNFA, tKleeneNFA, tAlphabetNFA, tAppendNFA]

-- >>> test_all

-- QUICKCHECK -- 

