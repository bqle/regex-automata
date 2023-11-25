module RegExOperationsTest where

import NFA
import RegexParser (injectConcatSymbol, popStackUntil, regexToNFA, regexToRPN)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

-- TODO

tMatch =
  "match"
    ~: TestList
      []

tSplitBy =
  "splitBy"
    ~: TestList
      []

tFindFirst =
  "findFirst"
    ~: TestList
      []

tFindFirstIndex =
  "findFirstIndex"
    ~: TestList
      []

tFindAll =
  "findAll"
    ~: TestList
      []

tReplace =
  "replace"
    ~: TestList
      []

-- >>> test_all_regex_operations
-- Counts {cases = 0, tried = 0, errors = 0, failures = 0}
test_all_regex_operations :: IO Counts
test_all_regex_operations =
  runTestTT $
    TestList
      [ tMatch,
        tSplitBy,
        tFindFirst,
        tFindFirstIndex,
        tFindAll,
        tReplace
      ]
