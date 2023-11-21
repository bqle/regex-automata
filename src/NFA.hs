module NFA where 

import qualified Data.List as List
import Data.Set.Monad (Set, fromList
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
epsilon = '\0'

makeTransition :: Transition -> Char -> String -> [String]
makeTransition t a s  = nub $ fromMaybe [] (Data.Map.lookup (s, a) t)
  ++ fromMaybe [] (Data.Map.lookup (s, epsilon) t)

-- | Finite NFA with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
--  Assumptions: 1 initial state, 1 accepting state
data NFA = NFA
  { uuid :: Int         -- ^ uuid for the DFA
  , initial    :: String              -- ^ Initial State
  , transition :: Transition      -- ^ Change state with a context.
  , accepting  :: String             -- ^ Accepting subset as a predicate.
  }

exampleNFA :: NFA 
exampleNFA = NFA {
  uuid = 0,
  initial = "0", 
  transition = Data.Map.fromList [(("0", 'a'), ["0"])],
  accepting = "0"
}

findNextStates transition states a = 
      nub $ concatMap (makeTransition transition a) states

-- Run an NFA & get the final states
run ::  NFA -> [Char] -> [String]
run NFA {uuid, initial, transition, accepting} = 
  foldl (findNextStates transition) 
    (nub (initial : makeTransition transition epsilon initial))
  where 
    findNextStates transition states a = 
      nub $ concatMap (makeTransition transition a) states


-- >>> run exampleNFA "a"
-- ["1","2","3"]

-- | Show instance
instance Show NFA where
  show NFA{uuid, initial, transition, accepting} 
    = "{  uuid: " ++ show uuid ++ " ,\n" ++ 
      "   initial" ++ show initial ++ " ,\n" ++ 
      "   transitions: " ++ show transition ++ " ,\n" ++
      "   accepting:" ++ show accepting ++ "\n }"

accept :: NFA -> [Char] -> Bool
accept nfa@NFA {initial, transition, accepting} s = 
  (accepting `elem`) $ run nfa s 

-- >>> accept exampleNFA "a"
-- True
-- >>> accept exampleNFA "ab"
-- False

-- | Attach the uuid to every state, update transitions accordingly
attachUUID :: NFA -> NFA
attachUUID NFA {uuid, initial, transition, accepting} =
  NFA uuid newInitial newTransitions newAccepting
  where 
    newInitial = initial ++ show uuid
    newAccepting = accepting ++ show uuid
    newTransitions = foldrWithKey (\(state, char) val
      -> Data.Map.insert (state ++ show uuid, char) 
        (List.map (++ show uuid) val)) Data.Map.empty transition
  
-- >>> attachUUID exampleNFA
-- {  uuid: 0 ,
--    initial["00"] ,
--    transitions: fromList [(("00",'a'),["0"])] ,
--    accepting:["00"]
--  }

-- | Union two transitions
unionTransitions :: Transition -> Transition -> Transition
unionTransitions t1 t2 = 
  foldrWithKey (\(state, char) dest acc -> 
    Data.Map.insert (state, char) (
      nub (fromMaybe [] (Data.Map.lookup (state, char) acc) ++ dest)
    ) acc
  ) t2 t1

-- | Create fully bipartite graph from two lists of vertices
--   We need original transitions because accepting states may already have 
--   e-transitions
bipartiteTransitions :: Transition -> [String] -> [String] -> Transition
bipartiteTransitions transOrig s1 s2 = 
  foldl (\acc acceptS -> 
    foldl (\acc initS -> 
      Data.Map.insert (acceptS, epsilon) (
        nub (initS :
          fromMaybe [] (Data.Map.lookup (acceptS, epsilon) acc) 
          )
      ) acc 
    ) acc s2 
  ) Data.Map.empty s1

-- | TODO: test attach UUID should not change semantics of an NFA

-- { CORE NFA Operations }

-- | unit NFA accepts a set of chars 
alphabet :: [Char] -> NFA
alphabet ls = 
  let
    initSt = "i"
    accSt = "e"
    transitions = foldl 
      (\acc v -> Data.Map.insert (initSt, v) [accSt] acc) Data.Map.empty ls
    in 
    NFA 0 initSt transitions accSt

-- | append 2 NFA (ab)
append :: NFA -> NFA -> NFA
append nfa1 nfa2 = 
  NFA 0 init1 newTransitions accept2
  where 
    NFA uuid1 init1 trans1 accept1 = attachUUID nfa1 
    NFA uuid2 init2 trans2 accept2 = attachUUID nfa2
    newConnections = bipartiteTransitions trans1 [accept1] [init2]
    newTransitions = unionTransitions newConnections (unionTransitions trans1 trans2)

-- >>> appendNFA exampleNFA exampleNFA{uuid="2"}
-- {  uuid: ztcck ,
--    initial["00"] ,
--    transitions: fromList [(("00",'\NUL'),["02"]),(("00",'a'),["0"]),(("02",'a'),["0"])] ,
--    accepting:["02"]
--  }

-- | alternate 2 NFA (a + b) (basically OR)
alternate :: NFA -> NFA -> NFA
alternate nfa1 nfa2 =  
    let 
      newInitSt = "i"
      newAccSt = "e"
      newConnections = unionTransitions
        (bipartiteTransitions Data.Map.empty [newInitSt] [init1, init2])
        (bipartiteTransitions 
          (Data.Map.union trans1 trans2) [accept1 , accept2] [newAccSt])
      newTransitions = unionTransitions newConnections 
        (unionTransitions trans1 trans2) in

      NFA 0 newInitSt newTransitions newAccSt 
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
kleene :: NFA -> NFA
kleene nfa = 
  let 
    newInitSt = "i"
    newAccSt = "e"
    NFA _ init trans accept = attachUUID nfa
    newTransitions = 
      unionTransitions 
        (bipartiteTransitions Data.Map.empty [newInitSt] [newAccSt, init])
        (bipartiteTransitions trans [accept] [newAccSt, init]) in
  NFA 0 newInitSt (unionTransitions newTransitions trans) newAccSt

-- >>> kleene exampleNFA
-- {  uuid: 0 ,
--    initial"i" ,
--    transitions: fromList [(("00",'\NUL'),["00"]),(("00",'a'),["0"]),(("i",'\NUL'),["00"])] ,
--    accepting:"e"
--  }
