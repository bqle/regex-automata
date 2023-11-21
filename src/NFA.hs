module NFA where 

import qualified Data.List as List
import Data.Set.Monad (Set, 
  fromList
  , empty) -- Note, we are using Data.Set.Monad instead of regular Set
import Data.List (nub)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC
import Data.Foldable          (foldlM)
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, lookup, empty, foldrWithKey, insert)

-- | (current state, new char) -> new states
type Transition = Map (String, Char) [String]

emptyTransition = '\0'

makeTransition :: Transition -> Char -> String -> [String]
makeTransition t a s  = fromMaybe [] (Data.Map.lookup (s, a) t)
  ++ fromMaybe [] (Data.Map.lookup (s, emptyTransition) t)

-- | Finite NFA with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
--  Assumptions: 1 initial state, 1 accepting state
data NFA = NFA
  { uuid :: String                   -- ^ uuid for the DFA
  , initial    :: [String]              -- ^ Initial State
  , transition :: Transition      -- ^ Change state with a context.
  , accepting  :: [String]             -- ^ Accepting subset as a predicate.
  }

exampleNFA :: NFA 
exampleNFA = NFA {
  uuid = "0",
  initial = ["0"], 
  transition = Data.Map.fromList [(("0", 'a'), ["1", "2", "3"])],
  accepting = ["0"]
}

-- Run an NFA & get the final states
run ::  NFA -> [Char] -> [String]
run NFA {initial, transition, accepting} = 
  foldl (findNextState transition) initial 
  where 
    findNextState transition states a = 
      concatMap (makeTransition transition a) states


-- >>> run exampleNFA "a"
-- ["1","2","3"]

type DFA = NFA 

exampleDFA :: DFA
exampleDFA = NFA {
  uuid = "0",
  initial = ["0"], 
  transition = Data.Map.fromList [(("0", 'a'), ["0"])],
  accepting = ["0"]
}

-- | Show instance
instance Show NFA where
  show NFA{uuid, initial, transition, accepting} 
    = "{  uuid: " ++ uuid ++ " ,\n" ++ 
      "   initial" ++ show initial ++ " ,\n" ++ 
      "   transitions: " ++ show transition ++ " ,\n" ++
      "   accepting:" ++ show accepting ++ "\n }"

-- >>> run exampleDFA "a"
-- ["0"]

accept :: NFA -> [Char] -> Bool
accept nfa@NFA {initial, transition, accepting} s = 
  any (`elem` accepting) $ run nfa s 

-- >>> accept exampleDFA "a"
-- True
-- >>> accept exampleDFA "ab"
-- False

-- | Attach the uuid to every state, update transitions accordingly
attachUUID :: NFA -> NFA
attachUUID NFA {uuid, initial, transition, accepting} =
  NFA uuid newInitial newTransitions newAccepting
  where 
    newInitial = map (++ uuid) initial
    newAccepting = map (++ uuid) accepting
    newTransitions = foldrWithKey (\(state, char) 
      -> Data.Map.insert (state ++ uuid, char)) Data.Map.empty transition
  
-- >>> attachUUID exampleNFA
-- {  uuid: 0 ,
--    initial["00"] ,
--    transitions: fromList [(("00",'a'),["1","2","3"])] ,
--    accepting:["00"]
--  }

-- | Union two transitions
unionTransitions :: Transition -> Transition -> Transition
unionTransitions t1 t2 = 
  foldrWithKey Data.Map.insert t2 t1

-- | TODO: test attach UUID should not change semantics of an NFA

-- { CORE NFA Operations }
-- | append 2 NFA (ab)
appendNFA :: NFA -> NFA -> NFA
appendNFA aut1 aut2 = 
  NFA newUUID init1 newTransitions accept2
  where 
    NFA uuid1 init1 trans1 accept1 = attachUUID aut1
    NFA uuid2 init2 trans2 accept2 = attachUUID aut2
    newUUID = uuid1 ++ uuid2 -- TODO : How to get random UUID?
    newConnections = -- add e-transitions
      foldl (\acc acceptS -> 
        foldl (\acc initS -> 
          Data.Map.insert (acceptS, emptyTransition) (
            fromMaybe [] (Data.Map.lookup (acceptS, emptyTransition) trans1)
            ++ [initS]) acc 
        ) acc init2 
      ) Data.Map.empty accept1
    newTransitions = unionTransitions newConnections (unionTransitions trans1 trans2)

-- >>> appendNFA exampleDFA exampleDFA{uuid="2"}
-- {  uuid: 02 ,
--    initial["00"] ,
--    transitions: fromList [(("00",'\NUL'),["02"]),(("00",'a'),["0"]),(("02",'a'),["0"])] ,
--    accepting:["02"]
--  }

-- | alternate 2 NFA (a + b)

-- | kleene-star (a*)



-- 
