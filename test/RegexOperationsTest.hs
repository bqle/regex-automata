module RegexOperationsTest where

import NFA
import RegexOperations
  ( findAll,
    findFirst,
    findFirstIndex,
    replace,
    splitBy,
    subset, 
  )
import RegexParser (injectConcatSymbol, popStackUntil, regexToNFA, regexToRPN)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

-- TODO
-- popStackUntil "abcdef" (== 'c') True ~?= ("ab", "def"),

tFindFirst =
  "match"
    ~: TestList
      [ findFirst "ab" "cccccabccccc" False ~?= Just "ab",
        findFirst "(ab)*" "cccccabccccc" False ~?= Just "ab",
        findFirst "ababab" "cccccabababccccc" False ~?= Just "ababab",
        findFirst "cdf" "cccccccdaaaaaaa" False ~?= Nothing,
        findFirst "*" "asd" False ~?= Nothing,
        findFirst "brandon|benjamin" "benja brandon min" False ~?= Just "brandon",
        findFirst "(a|b|c)(d|e|f)d" "lkjwoimabcfdasdkj" False ~?= Just "cfd",
        findFirst "(a|b|c)(d|e|f)d*" "lkjwoimabcfdasdkj" False ~?= Just "cf",
        findFirst "(a|b|c)(d|e|f)d" "lkjwoimabcfdasdkj" True ~?= Just "cfd",
        findFirst "b*" "aaaaaaaaabbbbbbbbccccccccc" True ~?= Just "bbbbbbbb",
        findFirst "b*" "aaaaaaaaabbbbbbbbccccccccc" False ~?= Just "b",
        findFirst "b*" "aabbbb" False ~?= Just "b",
        findFirst "b*" "aabbbb" True ~?= Just "bbbb"
      ]

-- >>> runTestTT tFindFirst
-- Counts {cases = 13, tried = 13, errors = 0, failures = 0}

tSplitBy =
  "splitBy"
    ~: TestList
      [ splitBy "," "a,b,c,d" ~?= Just ["a", "b", "c", "d"],
        splitBy "a" "aaba" ~?= Just ["", "", "b", ""],
        splitBy "a*" "aaba" ~?= Just ["", "b", ""],
        splitBy ",|;| " "a,b,c;d;f g h" ~?= Just ["a", "b", "c", "d", "f", "g", "h"],
        splitBy "a" "" ~?= Just [],
        splitBy "|" "asd" ~?= Nothing,
        splitBy ",," "a,,b,,c,,d,," ~?= Just ["a", "b", "c", "d", ""]
      ]

tFindFirstIndex =
  "findFirstIndex"
    ~: TestList
      [ findFirstIndex "ab" "cccccabccccc" ~?= Just 5,
        findFirstIndex "(ab)*" "cccccabccccc" ~?= Just 5,
        findFirstIndex "ababab" "ccccabababccccc" ~?= Just 4,
        findFirstIndex "cdf" "cccccccdaaaaaaa" ~?= Nothing,
        findFirstIndex "*" "asd" ~?= Nothing,
        findFirstIndex "brandon|benjamin" "benja brandon min" ~?= Just 6,
        findFirstIndex "(a|b|c)(d|e|f)d" "lkjwoimabcfdasdkj" ~?= Just 9,
        findFirstIndex "(a|b|c)(d|e|f)d*" "lkjwoimabcfdasdkj" ~?= Just 9,
        findFirstIndex "b*" "aaaaaaaaabbbbbbbbccccccccc" ~?= Just 10,
        findFirstIndex "a" "abc" ~?= Just 0,
        findFirstIndex "c" "abc" ~?= Just 2
      ]

tFindAll =
  "findAll"
    ~: TestList
      [ findAll "a" "abcabc"
          ~?= Just
            ["a", "a"],
        findAll
          "alice|bob"
          "alice charlie bob bob charlie alice alice bob"
          ~?= Just ["alice", "bob", "bob", "alice", "alice", "bob"],
        findAll "a*" "aaaabcdaaddd" ~?= Just ["aaaa", "aa"],
        findAll "a" "bbbbbbbb" ~?= Just [],
        findAll "|" "asdad" ~?= Nothing,
        findAll "abc|bcd|def" "abcdef" ~?= Just ["abc", "bcd", "def"]
      ]

tReplace =
  "replace"
    ~: TestList
      [ replace "abc" "abcdef" "def" ~?= Just "defdef",
        replace "a" "bbb" "xyz" ~?= Just "bbb",
        replace "a*" "bbbaaaaccc" "d" ~?= Just "bbbdccc",
        replace "alice|bob" "charlie alice bob charlie alice alice bob" "daniel"
          ~?= Just "charlie daniel daniel charlie daniel daniel daniel",
        replace "|" "asdasd" "asd" ~?= Nothing
      ]

tSubset = 
  "subset" 
    ~: TestList
      [ subset "abc" "abc" ~?= Just True,
        subset "a" "a|b" ~?= Just True, 
        subset "a|b" "a|b|c" ~?= Just True, 
        subset "aaaa" "a*" ~?= Just True, 
        subset "abc|abc" "abc" ~?= Just True, 
        subset "abcc" "abc*" ~?= Just True,
        subset "abcabc" "(abc)*" ~?= Just True,
        subset "a*b*c" "a*a*b*c*" ~?= Just True,
        subset "abcd" "abc" ~?= Just False, 
        subset "a|d" "a|b|c" ~?= Just False, 
        subset "b*" "a|b|c" ~?= Just False
      ]
-- >>> test_all_regex_operations
-- Counts {cases = 0, tried = 0, errors = 0, failures = 0}
test_all_regex_operations :: IO Counts
test_all_regex_operations =
  runTestTT $
    TestList
      [ tSplitBy,
        tFindFirst,
        tFindFirstIndex,
        tFindAll,
        tReplace,
        tSubset
      ]
