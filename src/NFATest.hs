import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import qualified NFA
import NFA (NFA, alphabet, kleene, accept, transition, epsilon, makeTransition)

lowercaseChars = ['a'..'b']

alphabetNFA :: NFA 
alphabetNFA = alphabet lowercaseChars

tUnitNFA = 
  "unitNFA" ~: TestList [
    accept alphabetNFA "a" ~?= True,
    accept alphabetNFA "e" ~?= True,
    accept alphabetNFA "x" ~?= True,
    accept alphabetNFA "ab" ~?= False,
    accept alphabetNFA "1" ~?= False
  ]

kleeneNFA :: NFA
kleeneNFA = kleene alphabetNFA

tKleeneNFA = 
  "kleeneNFA" ~: TestList [
    accept kleeneNFA "a" ~?= True,
    accept kleeneNFA "b" ~?= True,
    accept kleeneNFA "aa" ~?= True,
    accept kleeneNFA "aabb" ~?= True
  ]

-- >>> kleeneNFA
-- {  uuid: 0 ,
--    initial"i" ,
--    transitions: fromList [(("e0",'\NUL'),["i0","e"]),(("i",'\NUL'),["i0","e"]),(("i0",'a'),["e"]),(("i0",'b'),["e"])] ,
--    accepting:"e"
--  }
