import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck qualified as QC

import DFATest
import NFATest
import RegexOperationsTest
import RegexParserTest

main :: IO ()
main = do 
  putStrLn "--- NFA Tests ---"
  QC.quickCheck prop_bipartiteTrans
  QC.quickCheck prop_alternateStillAccepts
  QC.quickCheck prop_appendStillAccepts
  QC.quickCheck prop_kleeneStillAccepts
  QC.quickCheck prop_findAcceptingString
  test_all_nfa

  putStrLn ""
  putStrLn "--- DFA Tests ---"
  QC.quickCheck prop_DFAiffNFA
  test_all_dfa

  putStrLn ""
  putStrLn "--- Regex Parser Tests ---"
  QC.quickCheck (QC.forAll genRegExString prop_rpn_correct_length)
  QC.quickCheck (QC.forAll genRegExString prop_parseable_regex)
  test_all_regex_parser
  return ()

-- >>> main
