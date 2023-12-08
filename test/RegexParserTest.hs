module RegexParserTest where

import Control.Monad qualified as Monad
import Data.Maybe
import NFA
import RegexParser (injectConcatSymbol, popStackUntil, regexToNFA, regexToRPN)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

tPopStackUntil =
  "popStackUntil" ~:
    TestList
      [ popStackUntil "abcdef" (== 'c') True ~?= ("ab", "def"),
        popStackUntil "abcdef" (== 'c') False ~?= ("ab", "cdef"),
        popStackUntil "" (== 'a') False ~?= ("", ""),
        popStackUntil "a" (== 'a') True ~?= ("", ""),
        popStackUntil "a" (== 'a') False ~?= ("", "a"),
        popStackUntil "aaaa" (== 'b') True ~?= ("aaaa", "")
      ]

tInjectConcatSymbol :: Test
tInjectConcatSymbol =
  "injectConcatSymbol" ~:
    TestList
      [ injectConcatSymbol "abcdef" ~?= "a@b@c@d@e@f",
        injectConcatSymbol "" ~?= "",
        injectConcatSymbol "a" ~?= "a",
        injectConcatSymbol "ab(cd)ef" ~?= "a@b@(c@d)@e@f",
        injectConcatSymbol "a|b(cd)e*f" ~?= "a|b@(c@d)@e*@f"
      ]

tRegexToRPN :: Test
tRegexToRPN =
  "regexToRPN" ~:
    TestList
      [ regexToRPN "ab" ~?= "ab@",
        regexToRPN "abc" ~?= "abc@@",
        regexToRPN "a*|b*" ~?= "a*b*|",
        regexToRPN "(abc)*" ~?= "abc@@*",
        regexToRPN "((()))" ~?= ""
      ]

-- TODO

regexToNFATestHelper regex str =
  regexToNFA regex
    >>= ( \x ->
            Just (NFA.accept x str)
        )

tRegexToNFA =
  "regexToNFA" ~:
    TestList
      [ regexToNFATestHelper "a|b*" "" ~?= Just True,
        regexToNFATestHelper "a|b*" "a" ~?= Just True,
        regexToNFATestHelper "a|b*" "bbbbb" ~?= Just True,
        regexToNFATestHelper "a|b*" "ab" ~?= Just False,
        regexToNFATestHelper "(a|b)*" "abababbbbaa" ~?= Just True,
        regexToNFATestHelper "(a|b)*" "ababcabbbbaa" ~?= Just False,
        regexToNFATestHelper "(cis)|(552)" "cis" ~?= Just True,
        regexToNFATestHelper "(cis)|(552)" "552" ~?= Just True,
        regexToNFATestHelper "((cis)|(552))*" "cis552ciscis552" ~?= Just True,
        regexToNFATestHelper "((a)" "a" ~?= Nothing,
        regexToNFATestHelper "" "" ~?= Just False
      ]

-- >>> test_all_regex_parser
-- Counts {cases = 27, tried = 27, errors = 0, failures = 0}
test_all_regex_parser :: IO Counts
test_all_regex_parser =
  runTestTT $
    TestList
      [ tPopStackUntil,
        tInjectConcatSymbol,
        tRegexToRPN,
        tRegexToNFA
      ]

(+++) x y z = x ++ y ++ z

genRegExString = QC.sized gen
  where
    gen :: Int -> QC.Gen String
    gen n =
      QC.frequency
        [ (1, QC.elements ["a", "b", "c"]),
          (n, Monad.liftM2 (++) (gen (n `div` 2)) (gen (n `div` 2))),
          (n, Monad.liftM3 (+++) (return "(") (gen (n `div` 2)) (return ")")),
          (n, Monad.liftM2 (++) (gen (n `div` 2)) (return "*")),
          (n, Monad.liftM3 (+++) (gen (n `div` 2)) (return "|") (gen (n `div` 2)))
        ]

-- >>> QC.sample' genRegExString
-- ["b","((a))","(b)","b*|(a)*","(a**)|(b)b*","(b)b|c*ab*","(c*)|c*(c)","(a|c)|b|ba*a*|c*","(a**)|((c))|cb*a*c|a|(b)|(c|ca|c)","aa**b*|((b))|(cbb)|a|b**","((ca|a)|cac*)"]

prop_rpn_correct_length :: String -> Bool
prop_rpn_correct_length str =
  length (regexToRPN str)
    == length (filter (\x -> x /= '(' && x /= ')') (injectConcatSymbol str))

-- >>> QC.quickCheck (QC.forAll genRegExString prop_rpn_correct_length)

-- Does gnerated regex return None, if so FAIL
prop_parseable_regex :: String -> Bool
prop_parseable_regex str = isJust (regexToNFA str)

-- >>> QC.quickCheck (QC.forAll genRegExString prop_parseable_regex)

-- findAcceptingString
main :: IO ()
main = do
  _ <- QC.quickCheck (QC.forAll genRegExString prop_rpn_correct_length)
  _ <- QC.quickCheck (QC.forAll genRegExString prop_rpn_correct_length)
  putStrLn "Finished Parser QC"
