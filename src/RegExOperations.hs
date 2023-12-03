module RegexOperations
  ( findFirst,
    findFirstIndex,
    findAll,
    splitBy,
    replace,
    subset,
  )
where

import Data.Maybe (isJust, fromJust)
import NFA (NFA, accept)
import DFA (convert, isSubset)
import DFA 
import RegexParser (regexToNFA)

import Data.Set qualified as Set
import Data.Set
import Data.List qualified as List
import NFA 
import RandomString

takeWhileAcc :: ([a] -> Bool) -> [a] -> [a] -> Maybe [a]
takeWhileAcc f acc [] = if f acc then Just acc else Nothing
takeWhileAcc f acc (x : xs) =
  if f (acc ++ [x])
    then Just (acc ++ [x])
    else takeWhileAcc f (acc ++ [x]) xs

-- Once bool is "true", continue taking until it becomes false again
takeWhileAccMax :: ([a] -> Bool) -> [a] -> [a] -> Bool -> Maybe [a]
takeWhileAccMax f acc [] endFlag = if f acc || endFlag then Just acc else Nothing
takeWhileAccMax f acc (x : xs) endFlag
  | f (acc ++ [x]) = takeWhileAccMax f (acc ++ [x]) xs True
  | endFlag = Just acc
  | otherwise = takeWhileAccMax f (acc ++ [x]) xs False

runTailUntilJust :: ([a] -> Maybe [a]) -> [a] -> Maybe [a]
runTailUntilJust f (x : xs) = case f (x : xs) of
  Nothing -> runTailUntilJust f xs
  Just x -> Just x
runTailUntilJust f [] = if isJust (f []) then Just [] else Nothing

passthroughAccept :: NFA -> Bool -> String -> Maybe String
passthroughAccept nfa max str =
  if max
    then takeWhileAccMax (NFA.accept nfa) "" str False
    else takeWhileAcc (NFA.accept nfa) "" str

findFirst :: String -> String -> Bool -> Maybe String
findFirst regex str max = do
  nfa <- regexToNFA regex
  runTailUntilJust (passthroughAccept nfa max) str

-- >>> findFirst "a*" "bbbbaaaaaaaasdas" True
-- Just "aaaaaaaa"

splitBy :: String -> String -> Maybe [String]
splitBy regex str = undefined

findFirstIndex :: String -> String -> Maybe Int
findFirstIndex regex str = undefined

findAll :: String -> String -> Maybe [String]
findAll regex str = undefined

replace :: String -> String -> String -> Maybe String
replace regex str newStr = undefined

subset :: String -> String -> Maybe Bool
subset str1 str2 = do
  nfa1 <- regexToNFA str1
  nfa2 <- regexToNFA str2
  return $ convert nfa1 `isSubset` convert nfa2
