module RegexParser where

import Data.Bifunctor
import Data.Char (isAlpha)
import NFA (NFA, accept, alphabet, alternate, append, kleene, neverAcceptNFA, uuid)

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

popStackUntil ::
  String ->
  (Char -> Bool) ->
  Bool ->
  (String, String)
popStackUntil stack predicate removeFlaggedElem =
  let value =
        foldl
          ( \acc x ->
              if predicate x && thrd acc
                then
                  if removeFlaggedElem
                    then
                      ( frst acc,
                        scnd acc,
                        False
                      )
                    else
                      ( frst acc,
                        scnd acc ++ [x],
                        False
                      )
                else
                  if thrd acc
                    then
                      ( frst acc ++ [x],
                        scnd acc,
                        True
                      )
                    else
                      ( frst acc,
                        scnd
                          acc
                          ++ [x],
                        False
                      )
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
          if not
            ( elem x afterNoConcat
                || elem hd beforeNoConcat
            )
            then x : '@' : hd : tl
            else x : hd : tl
    )
    []

regexToRPN :: String -> String
regexToRPN regex =
  let parsed =
        foldl
          parseCharacter
          ([], [])
          (injectConcatSymbol regex)
   in uncurry (++) parsed
  where
    parseCharacter :: (String, String) -> Char -> (String, String)
    parseCharacter (outputQueue, operatorStack) c
      | c `elem` operators
          && peekStack operatorStack == '(' =
          (outputQueue, c : operatorStack)
      | c `elem` operators =
          let updatedStack =
                popStackUntil
                  operatorStack
                  (\op -> precedence c <= precedence op)
                  False
           in bimap (outputQueue ++) (c :) updatedStack
      | c == ')' =
          let updatedStack =
                popStackUntil
                  operatorStack
                  (== '(')
                  True
           in first (outputQueue ++) updatedStack
      | c == '(' = (outputQueue, c : operatorStack)
      | otherwise = (outputQueue ++ [c], operatorStack)

takeForBinary :: [a] -> Maybe (a, a, [a])
takeForBinary nfas = case nfas of
  x : y : arr -> Just (y, x, arr)
  _ -> Nothing

takeForUnary :: [a] -> Maybe (a, [a])
takeForUnary nfas = case nfas of
  x : arr -> Just (x, arr)
  _ -> Nothing

unwrapSingleElem :: Maybe [a] -> Maybe a
unwrapSingleElem (Just [x]) = Just x
unwrapSingleElem _ = Nothing

rpnToNFA :: String -> Maybe NFA
rpnToNFA rpn =
  if null rpn
    then Just neverAcceptNFA
    else
      unwrapSingleElem
        ( fst
            (foldl parseCharacter (Just [], 0) rpn)
        )
  where
    parseCharacter :: (Maybe [NFA], Int) -> Char -> (Maybe [NFA], Int)
    parseCharacter (Nothing, _) _ = (Nothing, 0)
    parseCharacter (Just stack, count) c
      | c `elem` operators = case c of
          '*' ->
            ( takeForUnary stack
                >>= (\x -> Just (kleene (fst x) {uuid = count} : snd x)),
              count + 1
            )
          '|' ->
            ( takeForBinary stack
                >>= (\x -> Just (alternate (frst x) (scnd x) {uuid = count} : thrd x)),
              count + 1
            )
          '@' ->
            ( takeForBinary stack
                >>= (\x -> Just (append (frst x) (scnd x) {uuid = count} : thrd x)),
              count + 1
            )
          _ -> (Nothing, count)
      | otherwise = (Just ((alphabet [c]) {uuid = count} : stack), count + 1)

regexToNFA :: String -> Maybe NFA
regexToNFA regex = rpnToNFA (regexToRPN regex)
