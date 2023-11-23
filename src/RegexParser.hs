module RegexParser () where

import Data.Bifunctor
import Data.Char (isAlpha)

-- import NFA (NFA)

frst :: (a, b, c) -> a
frst (a, _, _) = a

scnd :: (a, b, c) -> b
scnd (_, b, _) = b

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

operators = "*|@"

precedence :: Char -> Int
precedence c = case c of
  '*' -> 1
  '@' -> 2
  '|' -> 3
  _ -> 4

peekStack :: String -> Char
peekStack (x : xs) = x
peekStack [] = 'ε'

popStackUntil :: String -> (Char -> Bool) -> Bool -> (String, String)
popStackUntil stack predicate removeFlaggedElem =
  let value =
        foldl
          ( \acc x ->
              if predicate x && thrd acc
                then
                  if removeFlaggedElem
                    then (frst acc, scnd acc, False)
                    else (frst acc, scnd acc ++ [x], False)
                else
                  if thrd acc
                    then (frst acc ++ [x], scnd acc, True)
                    else (frst acc, scnd acc ++ [x], False)
          )
          ("", "", True)
          stack
   in (frst value, scnd value)

beforeNoConcat = "*|)"

afterNoConcat = "(|"

injectConcatSymbol :: String -> String
injectConcatSymbol =
  foldr
    ( \x acc -> case acc of
        [] -> [x]
        (hd : tl) ->
          if not (elem x afterNoConcat || elem hd beforeNoConcat)
            then x : '@' : hd : tl
            else x : hd : tl
    )
    []

regexToRPN :: String -> String
regexToRPN regex =
  let parsed = foldl parseCharacter ([], []) (injectConcatSymbol regex)
   in uncurry (++) parsed
  where
    parseCharacter :: (String, String) -> Char -> (String, String)
    parseCharacter (outputQueue, operatorStack) c
      | c `elem` operators && peekStack operatorStack == '(' = (outputQueue, c : operatorStack) -- done
      | c `elem` operators =
          let updatedStack = popStackUntil operatorStack (\op -> precedence c < precedence op) False
           in bimap (outputQueue ++) (c :) updatedStack
      | c == ')' =
          let updatedStack = popStackUntil operatorStack (== '(') True
           in first (outputQueue ++) updatedStack
      | c == '(' = (outputQueue, c : operatorStack) -- done
      | otherwise = (outputQueue ++ [c], operatorStack) -- done

-- >>> regexToRPN "a(a|b)*b"

-- >>> injectConcatSymbol "a(a|b)*b"
-- "a@(a|b)*@b"

-- rpnToNFA :: string -> NFA
-- rpnToNFA = undefined
