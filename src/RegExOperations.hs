module RegexOperations () where

import Data.Maybe (isJust)
import NFA (NFA, accept)
import RegexParser (regexToNFA)

takeWhileAcc :: ([a] -> Bool) -> [a] -> [a] -> Maybe [a]
takeWhileAcc f acc [] = if f acc then Just acc else Nothing
takeWhileAcc f acc (x : xs) = if f (acc ++ [x]) then Just (acc ++ [x]) else takeWhileAcc f (acc ++ [x]) xs

runTailUntilJust :: ([a] -> Maybe [a]) -> [a] -> Maybe [a]
runTailUntilJust f (x : xs) = case f (x : xs) of
  Nothing -> runTailUntilJust f xs
  Just x -> Just x
runTailUntilJust f [] = if isJust (f []) then Just [] else Nothing

passthroughAccept :: NFA -> String -> Maybe String
passthroughAccept nfa = takeWhileAcc (accept nfa) ""

findFirst :: String -> String -> Maybe String
findFirst regex str =
  let nfa = regexToNFA regex
   in case nfa of
        Nothing -> Nothing
        Just n -> runTailUntilJust (passthroughAccept n) str

-- >>> findFirst "(bob|j(o|c)hn)" "hi jchn my name is bob"
-- Just "jchn"

splitBy regex str = undefined

findFirstIndex :: String -> String -> int
findFirstIndex regex str = undefined

findAll :: String -> String -> [String]
findAll regex str = undefined

replace :: String -> String -> String -> String
replace regex str newStr = undefined
