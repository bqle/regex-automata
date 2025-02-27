module NFATest where

import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Data.List
import Data.Maybe
import NFA
import qualified NFA
import Data.Set (fromList )
import Data.Set qualified as Set
import Test.QuickCheck (quickCheckAll)
import Test.QuickCheck as QC
import Data.Map
import Control.Monad (replicateM)

lowercaseChars = ['a'..'z']

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
kleeneNFA = kleene (alphabetNFA, 0)

tKleeneNFA = 
  "kleeneNFA" ~: TestList [
    accept kleeneNFA "a" ~?= True,
    accept kleeneNFA "" ~?= True,
    accept kleeneNFA "b" ~?= True,
    accept kleeneNFA "aa" ~?= True,
    accept kleeneNFA "aabb" ~?= True
  ]

appendNFA :: NFA 
appendNFA = append (alphabetNFA, 0) (alphabetNFA, 1)

tAppendNFA = 
  "appendNFA" ~: TestList [
    accept appendNFA "a" ~?= False,
    accept appendNFA "" ~?= False,
    accept appendNFA "ba" ~?= True
  ]

alternateNFA :: NFA
alternateNFA = alternate (alphabetNFA, 2) (appendNFA, 3)

tAlternateNFA = 
  "alternateNFA" ~: TestList [
    accept alternateNFA "a" ~?= True,
    accept alternateNFA "ab" ~?= True,
    accept alternateNFA "abc" ~?= False,
    accept alternateNFA "" ~?= False
  ]

test_all_nfa :: IO Counts
test_all_nfa = runTestTT $ TestList [tAlternateNFA, tKleeneNFA, tAlphabetNFA, tAppendNFA]

-- >>> test_all_nfa
-- Counts {cases = 16, tried = 16, errors = 0, failures = 0}

-- QUICKCHECK -- 

-- Function to generate a list of count random strings with a given length 
generateRandomStringList :: Int -> Int -> Gen [String]
generateRandomStringList len count = replicateM count $ 
  replicateM len ( choose ('a', 'z'))

-- Generate list of count connectsion between nodes given nodes list
generateConnectionList :: [String] -> Int -> Gen [(String, Char, String)]
generateConnectionList strs count = replicateM count genConnection
  where 
    strLen = length strs
    genConnection = do
      index1 <- choose (0, strLen - 1)
      index2 <- choose (0, strLen- 1)
      c <- choose ('a', 'z')
      let str1 = strs !! index1
      let str2 = strs !! index2
      return (str1, c, str2)

instance Arbitrary NFA where
  arbitrary :: Gen NFA
  arbitrary = do
      nodes <- generateRandomStringList 3 10
      edges <- generateConnectionList (init : end : nodes) 10
      c <- choose ('a', 'z')
      fromI <- sublistOf nodes 
      toE <- sublistOf nodes
      let 
        newEdges = Prelude.map (init, c,) fromI ++ Prelude.map (,c , end) toE
          ++ edges
        newTrans = Prelude.foldl insertTransition Data.Map.empty newEdges in
        return $ Aut init newTrans end
      where 
        init = "i"
        end = "e"
  shrink :: NFA -> [NFA]
  shrink nfa@Aut{initial, transition, accepting}= 
    let smallerTrans = shrink transition in 
      Prelude.map (\newTrans -> Aut initial newTrans accepting) 
        smallerTrans

prop_bipartiteTrans :: [String] -> [String] -> Bool
prop_bipartiteTrans s1 s2 = countTransitions 
  (bipartiteTransitions Data.Map.empty (Set.fromList s1) (Set.fromList s2)) == 
    length (nub s1) * length (nub s2)

prop_findAcceptingString :: NFA -> Property 
prop_findAcceptingString nfa = 
  let a1 = findAcceptingString nfa in 
    isJust a1 ==> 
    let s = fromJust a1 in 
      accept nfa s

prop_alternateStillAccepts :: NFA -> NFA -> Property
prop_alternateStillAccepts n1 n2 = 
  let 
    a1 = findAcceptingString n1 in
  isJust a1 ==> 
  let s = fromJust a1 in 
    accept (alternate (n1,1) (n2, 2)) s

prop_appendStillAccepts :: NFA -> NFA -> Property
prop_appendStillAccepts n1 n2 = 
  let 
    a1 = findAcceptingString n1 
    a2 = findAcceptingString n2 in
  isJust a1  && isJust a2 ==> 
  let 
    s1 = fromJust a1 
    s2 = fromJust a2 in 
      accept (append (n1, 1) (n2, 2)) (s1 ++ s2)

prop_kleeneStillAccepts:: NFA -> Int -> Property
prop_kleeneStillAccepts n1 k = 
  let 
    a1 = findAcceptingString n1 in 
  isJust a1 ==> 
  let 
    s1 = fromJust a1 in
      accept (kleene (n1, 0)) (concat $ replicate k s1)

-- | IO 
mainTest :: IO ()
mainTest = do 
  _ <- QC.quickCheck prop_bipartiteTrans
  _ <- QC.quickCheck prop_alternateStillAccepts
  _ <- QC.quickCheck prop_appendStillAccepts
  _ <- QC.quickCheck prop_kleeneStillAccepts
  _ <- QC.quickCheck prop_findAcceptingString
  nfa <- QC.generate (arbitrary :: Gen NFA)
  let acceptStr = findAcceptingString nfa in
    case acceptStr of
      Nothing -> print "No accepting string"
      Just str -> do
        print (findAcceptingString nfa )
        print nfa
        print (accept nfa str)
  

