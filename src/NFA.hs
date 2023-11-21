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
import Data.Map (Map, fromList, lookup, empty, foldrWithKey, insert, union)
import RandomString (randomUUID)

-- | (current state, new char) -> new states
type Transition = Map (String, Char) [String]

-- | Represents the epsilon transitions in NFAs
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
  transition = Data.Map.fromList [(("0", 'a'), ["0"])],
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

-- | Show instance
instance Show NFA where
  show NFA{uuid, initial, transition, accepting} 
    = "{  uuid: " ++ uuid ++ " ,\n" ++ 
      "   initial" ++ show initial ++ " ,\n" ++ 
      "   transitions: " ++ show transition ++ " ,\n" ++
      "   accepting:" ++ show accepting ++ "\n }"

accept :: NFA -> [Char] -> Bool
accept nfa@NFA {initial, transition, accepting} s = 
  any (`elem` accepting) $ run nfa s 

-- >>> accept exampleNFA "a"
-- True
-- >>> accept exampleNFA "ab"
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
--    transitions: fromList [(("00",'a'),["0"])] ,
--    accepting:["00"]
--  }

-- | Union two transitions
unionTransitions :: Transition -> Transition -> Transition
unionTransitions t1 t2 = 
  foldrWithKey Data.Map.insert t2 t1

-- | Create fully bipartite graph from two lists of vertices
--   We need original transitions because accepting states may already have 
--   e-transitions
bipartiteTransitions :: Transition -> [String] -> [String] -> Transition
bipartiteTransitions transOrig s1 s2 = 
  foldl (\acc acceptS -> 
    foldl (\acc initS -> 
      Data.Map.insert (acceptS, emptyTransition) (
        fromMaybe [] (Data.Map.lookup (acceptS, emptyTransition) transOrig)
        ++ [initS]) acc 
    ) acc s2 
  ) Data.Map.empty s1

-- | TODO: test attach UUID should not change semantics of an NFA

-- { CORE NFA Operations }
-- | append 2 NFA (ab)
appendNFA :: NFA -> NFA -> IO NFA
appendNFA nfa1 nfa2 = do
  newUUID <- randomUUID
  return $ NFA newUUID init1 newTransitions accept2
  where 
    NFA uuid1 init1 trans1 accept1 = attachUUID nfa1 
    NFA uuid2 init2 trans2 accept2 = attachUUID nfa2
    newConnections = bipartiteTransitions trans1 accept1 init2
    newTransitions = unionTransitions newConnections (unionTransitions trans1 trans2)

-- >>> appendNFA exampleNFA exampleNFA{uuid="2"}
-- {  uuid: ztcck ,
--    initial["00"] ,
--    transitions: fromList [(("00",'\NUL'),["02"]),(("00",'a'),["0"]),(("02",'a'),["0"])] ,
--    accepting:["02"]
--  }

-- | alternate 2 NFA (a + b) (basically OR)
alternateNFA :: NFA -> NFA -> IO NFA
alternateNFA nfa1 nfa2 = do 
    newUUID <- randomUUID
    newInitSt <- randomUUID
    newAccSt <- randomUUID
    let 
      newInit = [newInitSt] 
      newAccept = [newAccSt] 
      newConnections = unionTransitions
        (bipartiteTransitions Data.Map.empty newInit (init1 ++ init2))
        (bipartiteTransitions 
          (Data.Map.union trans1 trans2) (accept1 ++ accept2) newAccept)
      newTransitions = unionTransitions newConnections 
        (unionTransitions trans1 trans2) in

      return $ NFA newUUID newInit newTransitions newAccept
    where 
      NFA uuid1 init1 trans1 accept1 = attachUUID nfa1 
      NFA uuid2 init2 trans2 accept2 = attachUUID nfa2 
  
-- >>> alternateNFA exampleNFA exampleNFA
-- {  uuid: gmf ,
--    initial["opo"] ,
--    transitions: fromList [(("00",'\NUL'),["ewg"]),(("00",'a'),["0"]),(("opo",'\NUL'),["00"])] ,
--    accepting:["ewg"]
--  }

-- | kleene-star (a*)
kleeneNFA :: NFA -> IO NFA
kleeneNFA nfa@(NFA uuid init trans accept) = do
  newInitSt <- randomUUID
  newAccSt <- randomUUID
  let 
    newInit = [newInitSt]
    newAccept = [newAccSt]
    newTransitions = 
      unionTransitions 
        (bipartiteTransitions Data.Map.empty newInit (newAccept ++ init))
        (bipartiteTransitions trans accept (newAccept ++ init))
  return $ NFA uuid newInit (unionTransitions newTransitions trans) newAccept

-- >>> kleeneNFA exampleNFA
-- {  uuid: 0 ,
--    initial["ina"] ,
--    transitions: fromList [(("0",'\NUL'),["0"]),(("0",'a'),["0"]),(("ina",'\NUL'),["0"])] ,
--    accepting:["foa"]
--  }


-- 
