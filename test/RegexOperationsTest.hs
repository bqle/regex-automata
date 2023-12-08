module RegexOperationsTest where

import NFA
import RegexOperations
  ( findAll,
    findFirst,
    findFirstIndex,
    replace,
    replaceAll,
    splitBy,
    subset,
  )
import RegexParser (injectConcatSymbol, popStackUntil, regexToNFA, regexToRPN)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

tFindFirst =
  "findFirst" ~:
    TestList
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

tFindFirstIndex =
  "findFirstIndex" ~:
    TestList
      [ findFirstIndex "ab" "cccccabccccc" ~?= Just 5,
        findFirstIndex "(ab)*" "cccccabccccc" ~?= Just 5,
        findFirstIndex "ababab" "ccccabababccccc" ~?= Just 4,
        findFirstIndex "cdf" "cccccccdaaaaaaa" ~?= Nothing,
        findFirstIndex "*" "asd" ~?= Nothing,
        findFirstIndex "brandon|benjamin" "benja brandon min" ~?= Just 6,
        findFirstIndex "(a|b|c)(d|e|f)d" "lkjwoimabcfdasdkj" ~?= Just 9,
        findFirstIndex "(a|b|c)(d|e|f)d*" "lkjwoimabcfdasdkj" ~?= Just 9,
        findFirstIndex "b*" "aaaaaaaaabbbbbbbbccccccccc" ~?= Just 9,
        findFirstIndex "a" "abc" ~?= Just 0,
        findFirstIndex "c" "abc" ~?= Just 2
      ]

tSplitBy =
  "splitBy" ~:
    TestList
      [ splitBy "," "a,b,c,d" ~?= Just ["a", "b", "c", "d"],
        splitBy "a" "aaba" ~?= Just ["", "", "b", ""],
        splitBy "a*" "aaba" ~?= Just ["", "b", ""],
        splitBy ",|;| " "a,b,c;d;f g h" ~?= Just ["a", "b", "c", "d", "f", "g", "h"],
        splitBy "a" "" ~?= Just [""],
        splitBy "|" "asd" ~?= Nothing,
        splitBy ",," "a,,b,,c,,d,," ~?= Just ["a", "b", "c", "d", ""]
      ]

tFindAll =
  "findAll" ~:
    TestList
      [ findAll "a" "abcabc"
          ~?= Just
            ["a", "a"],
        findAll
          "alice|bob"
          "alice charlie bob bob charlie alice alice bob"
          ~?= Just ["alice", "bob", "bob", "alice", "alice", "bob"],
        findAll "a*" "aaaabcdaaddd" ~?= Just ["aaaa", "aaa", "aa", "a", "aa", "a"],
        findAll "a" "bbbbbbbb" ~?= Just [],
        findAll "|" "asdad" ~?= Nothing,
        findAll "abc|bcd|def" "abcdef" ~?= Just ["abc", "bcd", "def"],
        findAll "" "abc" ~?= Just []
      ]

tReplace =
  "replace" ~:
    TestList
      [ replace "abc" "abcdef" "def" ~?= Just "defdef",
        replace "a" "bbb" "xyz" ~?= Just "bbb",
        replace "a*" "bbbaaaaccc" "d" ~?= Just "bbbdccc",
        replace "alice|bob" "charlie alice bob charlie alice alice bob" "daniel"
          ~?= Just "charlie daniel bob charlie alice alice bob",
        replace "|" "asdasd" "asd" ~?= Nothing
      ]

tReplaceAll =
  "replaceAll" ~:
    TestList
      [ replaceAll "abc" "abcdef" "def" ~?= Just "defdef",
        replaceAll "a" "bbb" "xyz" ~?= Just "bbb",
        replaceAll "a*" "bbbaaaaccc" "d" ~?= Just "bbbdccc",
        replaceAll "alice|bob" "charlie alice bob charlie alice alice bob" "daniel"
          ~?= Just "charlie daniel daniel charlie daniel daniel daniel",
        replaceAll "|" "asdasd" "asd" ~?= Nothing
      ]

tSubset =
  "subset" ~:
    TestList
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
