module RegExParserTest where

-- accept alternateNFA "a" ~?= True,

import NFA
import RegexParser (injectConcatSymbol, popStackUntil, regexToNFA, regexToRPN)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

tPopStackUntil =
  "popStackUntil"
    ~: TestList
      [ popStackUntil "abcdef" (== 'c') True ~?= ("ab", "def"),
        popStackUntil "abcdef" (== 'c') False ~?= ("ab", "cdef"),
        popStackUntil "" (== 'a') False ~?= ("", ""),
        popStackUntil "a" (== 'a') True ~?= ("", ""),
        popStackUntil "a" (== 'a') False ~?= ("", "a"),
        popStackUntil "aaaa" (== 'b') True ~?= ("aaaa", "")
      ]

tInjectConcatSymbol :: Test
tInjectConcatSymbol =
  "injectConcatSymbol"
    ~: TestList
      [ injectConcatSymbol "abcdef" ~?= "a@b@c@d@e@f",
        injectConcatSymbol "" ~?= "",
        injectConcatSymbol "a" ~?= "a",
        injectConcatSymbol "ab(cd)ef" ~?= "a@b@(c@d)@e@f",
        injectConcatSymbol "a|b(cd)e*f" ~?= "a|b@(c@d)@e*@f"
      ]

tRegexToRPN :: Test
tRegexToRPN =
  "regexToRPN"
    ~: TestList
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
  "regexToNFA"
    ~: TestList
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
