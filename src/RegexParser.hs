module RegexParser () where

import Data.Char (isAlpha)

-- import NFA (NFA)

beforeNoConcat = "*|)"

afterNoConcat = "(|"

regexToRPN :: String -> String
regexToRPN regex = undefined

-- >>> injectConcatSymbol "abc(de)"
-- "a_b_c_(d_e)"
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

-- rpnToNFA :: string -> NFA
-- rpnToNFA = undefined
