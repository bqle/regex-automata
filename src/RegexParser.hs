module RegexParser () where

import Data.Bifunctor
import Data.Char (isAlpha)
import NFA (NFA, accept, alphabet, alternate, append, kleene)

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
          (outputQueue, c : operatorStack) -- done
      | c `elem` operators =
          let updatedStack =
                popStackUntil
                  operatorStack
                  (\op -> precedence c < precedence op)
                  False
           in bimap (outputQueue ++) (c :) updatedStack
      | c == ')' =
          let updatedStack =
                popStackUntil
                  operatorStack
                  (== '(')
                  True
           in first (outputQueue ++) updatedStack
      | c == '(' = (outputQueue, c : operatorStack) -- done
      | otherwise = (outputQueue ++ [c], operatorStack) -- done

-- >>> regexToRPN "a(a|b)*b"

-- >>> injectConcatSymbol "a(a|b)*b"
-- "a@(a|b)*@b"

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
rpnToNFA rpn = unwrapSingleElem (foldl parseCharacter (Just []) rpn)
  where
    parseCharacter :: Maybe [NFA] -> Char -> Maybe [NFA]
    parseCharacter Nothing _ = Nothing
    parseCharacter (Just stack) c
      | c `elem` operators = case c of
          '*' ->
            takeForUnary stack
              >>= (\x -> Just (kleene (fst x) : snd x))
          '|' ->
            takeForBinary stack
              >>= (\x -> Just (alternate (frst x) (scnd x) : thrd x))
          '@' ->
            takeForBinary stack
              >>= (\x -> Just (append (frst x) (scnd x) : thrd x))
          _ -> Nothing
      | otherwise = Just (alphabet [c] : stack)

regexToNFA :: String -> Maybe NFA
regexToNFA regex = rpnToNFA (regexToRPN regex)

-- "a(a|b)*b"

-- test :: Maybe Bool
test = regexToNFA "ab" >>= \x -> Just (NFA.accept x "b")

-- >>> regexToRPN "ab"
-- "ab@"

-- >>>  test
-- Just True

-- >>> append (alphabet ['a']) (alphabet ['b'])
-- >>> NFA.accept (append (alphabet ['a']) (alphabet ['b'])) "b"
-- {  uuid: 0 ,
--    initial"\"a4e167a76a05add8a8654c169b07b0447a916035aef602df103e8ae0fe2ff390\"" ,
--    transitions: fromList [(("\"5c88e7a226e11ad1204cb8d30cd5d6ff6cba69bc32da73134928e17c90c54086\"",'\NUL'),["\"a4e167a76a05add8a8654c169b07b0447a916035aef602df103e8ae0fe2ff390\""]),(("\"a4e167a76a05add8a8654c169b07b0447a916035aef602df103e8ae0fe2ff390\"",'a'),["\"5c88e7a226e11ad1204cb8d30cd5d6ff6cba69bc32da73134928e17c90c54086\""]),(("\"a4e167a76a05add8a8654c169b07b0447a916035aef602df103e8ae0fe2ff390\"",'b'),["\"5c88e7a226e11ad1204cb8d30cd5d6ff6cba69bc32da73134928e17c90c54086\""])] ,
--    accepting:"\"5c88e7a226e11ad1204cb8d30cd5d6ff6cba69bc32da73134928e17c90c54086\""
--  }
-- True
