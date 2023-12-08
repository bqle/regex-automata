module RegexOperations
  ( findFirst,
    findFirstIndex,
    findAll,
    splitBy,
    replace,
    subset,
    replaceAll,
  )
where

import DFA
import Data.List
import Data.List qualified as List
import Data.Maybe (fromJust, isJust)
import Data.Set
import Data.Set qualified as Set
import NFA
import RandomString
import RegexParser (regexToNFA)

takeWhileAcc :: ([a] -> Bool) -> [a] -> [a] -> Maybe ([a], [a])
takeWhileAcc f acc [] =
  if f acc
    then Just (acc, [])
    else Nothing
takeWhileAcc f acc (x : xs) =
  if f (acc ++ [x])
    then Just (acc ++ [x], xs)
    else takeWhileAcc f (acc ++ [x]) xs

-- Once bool is "true", continue taking until it becomes false again
takeWhileAccMax ::
  ([a] -> Bool) ->
  [a] ->
  [a] ->
  Bool ->
  Maybe ([a], [a])
takeWhileAccMax f acc [] endFlag =
  if f acc || endFlag
    then Just (acc, [])
    else Nothing
takeWhileAccMax f acc (x : xs) endFlag
  | f (acc ++ [x]) =
      takeWhileAccMax
        f
        (acc ++ [x])
        xs
        True
  | endFlag = Just (acc, x : xs)
  | otherwise =
      takeWhileAccMax
        f
        (acc ++ [x])
        xs
        False

passthroughAccept :: NFA -> Bool -> String -> Maybe (String, String)
passthroughAccept nfa max str =
  if max
    then takeWhileAccMax (NFA.accept nfa) "" str False
    else takeWhileAcc (NFA.accept nfa) "" str

runTailUntilJust ::
  ([a] -> Maybe ([a], [a])) ->
  ([a], [a]) ->
  Maybe ([a], [a])
runTailUntilJust f (x : xs, tl) =
  case f (x : xs) of
    Nothing -> runTailUntilJust f (xs, tl)
    Just x' -> Just x'
runTailUntilJust f ([], tl) =
  if isJust (f [])
    then Just ([], tl)
    else Nothing

findFirst :: String -> String -> Bool -> Maybe String
findFirst regex str max = do
  nfa <- regexToNFA regex
  runTailUntilJust
    (passthroughAccept nfa max)
    (str, [])
    >>= \x -> Just (fst x)

findFirstIndex :: String -> String -> Maybe Int
findFirstIndex regex str = do
  nfa <- regexToNFA regex
  foundString <-
    runTailUntilJust
      (passthroughAccept nfa False)
      (str, [])
      >>= \x -> Just (fst x)
  Data.List.findIndex
    (isPrefixOf foundString)
    (tails str)

findAllHelper :: NFA -> String -> [String]
findAllHelper nfa str =
  case runTailUntilJust
    (passthroughAccept nfa True)
    (str, []) of
    Nothing -> []
    Just ([], tl) -> []
    Just (x : xs, tl) -> (x : xs) : findAllHelper nfa (xs ++ tl)

findAll :: String -> String -> Maybe [String]
findAll regex str = do
  nfa <- regexToNFA regex
  Just (findAllHelper nfa str)

splitByHelper :: NFA -> String -> [String]
splitByHelper nfa str =
  case runTailUntilJust
    (passthroughAccept nfa True)
    (str, []) of
    Nothing -> [str]
    Just ([], tl) -> [tl]
    Just (found, tl) ->
      Data.List.take
        (length str - length found - length tl)
        str
        : splitByHelper nfa tl

splitBy :: String -> String -> Maybe [String]
splitBy regex str = do
  nfa <- regexToNFA regex
  Just (splitByHelper nfa str)

replace :: String -> String -> String -> Maybe String
replace regex str newStr = do
  nfa <- regexToNFA regex
  case runTailUntilJust
    (passthroughAccept nfa True)
    (str, []) of
    Nothing -> Just str
    Just (found, tl) ->
      Just
        ( Data.List.take
            (length str - length found - length tl)
            str
            ++ newStr
            ++ tl
        )

replaceAllHelper :: NFA -> String -> String -> String
replaceAllHelper nfa str newStr = case runTailUntilJust
  (passthroughAccept nfa True)
  (str, []) of
  Nothing -> str
  Just ([], tl) -> str
  Just (found, tl) ->
    Data.List.take
      (length str - length found - length tl)
      str
      ++ newStr
      ++ replaceAllHelper nfa tl newStr

replaceAll :: String -> String -> String -> Maybe String
replaceAll regex str newStr = do
  nfa <- regexToNFA regex
  Just (replaceAllHelper nfa str newStr)

subset :: String -> String -> Maybe Bool
subset str1 str2 = do
  nfa1 <- regexToNFA str1
  nfa2 <- regexToNFA str2
  return $ convert nfa1 `isSubset` convert nfa2
