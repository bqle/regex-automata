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

-- | Correspond to a state of the automata.
--  We need the Int to distinguish states that have the same name 
-- but are from different NFAs, when combined
data AutState = AutState {
  name:: String,
  nfaId :: Int
} deriving (Ord, Eq, Show)


-- | (current state, new char) -> new states
type Transition = Map (AutState, Char) [AutState]

-- | Represents the epsilon transitions in NFAs
epsilon = '\0'

makeTransition :: Transition -> Char -> AutState -> [AutState]
makeTransition t a s  = nub $ fromMaybe [] (Data.Map.lookup (s, a) t)

-- | Finite NFA with state `s`, alphabet `a` and a monadic context `m`.
--   The type parameters `s` and `a` are assumed to represent finite set
--  Assumptions: 1 initial state, 1 accepting state
data NFA = NFA
  { uuid :: Int, 
    initial    :: AutState,
    transition :: Transition,
    accepting  :: AutState
  }

exampleNFA :: NFA 
exampleNFA = NFA {
  uuid = 0,
  initial = AutState "0" 0, 
  transition = Data.Map.fromList [((AutState "0" 0, 'a'), [AutState "0" 0])],
  accepting = AutState "0" 0
}

-- | Find all transitions after taking the
findNextStates :: Transition -> [AutState] -> Char -> [AutState]
findNextStates transition states a = 
      nub $ exploreEpsilons transition 
        (nub $ concatMap (makeTransition transition a) states)

-- | Expand the states to include all reachable states thru epsilon transitions
exploreEpsilons :: Transition -> [AutState] -> [AutState]
exploreEpsilons transition states = 
  let
    immediateFrontier = nub $ concatMap (makeTransition transition epsilon) states
  in 
    case immediateFrontier of
      [] -> states
      _ -> exploreEpsilons transition immediateFrontier ++ states

-- Run an NFA & get the final states
run ::  NFA -> [Char] -> [AutState]
run NFA {uuid, initial, transition, accepting} = 
  foldl (findNextStates transition) 
    (nub (initial : makeTransition transition epsilon initial))


-- >>> run exampleNFA "a"
-- ["1","2","3"]

-- | Show instance
instance Show NFA where
  show NFA{uuid, initial, transition, accepting} 
    = "{  uuid: " ++ show uuid ++ " ,\n" ++ 
      "   initial: " ++ show initial ++ " ,\n" ++ 
      "   transitions: " ++ show transition ++ " ,\n" ++
      "   accepting:" ++ show accepting ++ "\n }"

accept :: NFA -> [Char] -> Bool
accept nfa@NFA {initial, transition, accepting} s = 
  (accepting `elem`) $ run nfa s 

-- >>> accept exampleNFA "a"
-- True
-- >>> accept exampleNFA "ab"
-- False

setNfaId :: Int -> AutState -> AutState
setNfaId id st = st{nfaId = id}

-- | Attach the uuid to every state, update transitions accordingly
attachUUID :: NFA -> NFA
attachUUID NFA {uuid, initial, transition, accepting} =
  NFA uuid newInitial newTransitions newAccepting
  where 
    newInitial = initial {nfaId=uuid}
    newAccepting = accepting {nfaId=uuid}
    newTransitions = foldrWithKey (\(state, char) val
      -> Data.Map.insert (state {nfaId=uuid}, char) 
        (List.map (setNfaId uuid) val)) Data.Map.empty transition
  
-- >>> attachUUID exampleNFA
-- {  uuid: 0 ,
--    initial["00"] ,
--    transitions: fromList [(("00",'a'),["0"])] ,
--    accept:["00"]
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
bipartiteTransitions :: Transition -> [AutState] -> [AutState] -> Transition
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
    initSt = AutState "i" 0
    accSt = AutState "e" 0
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
    newTransitions = 
      unionTransitions newConnections (unionTransitions trans1 trans2)

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
      newInitSt = AutState "i" 0
      newAccSt = AutState "e" 0
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
    newInitSt = AutState "i" 0
    newAccSt = AutState "e" 0
    NFA _ init trans accept = attachUUID nfa
    newTransitions = 
      unionTransitions 
        (bipartiteTransitions Data.Map.empty [newInitSt] [newAccSt, init])
        (bipartiteTransitions trans [accept] [newAccSt, init]) in
  NFA 0 newInitSt (unionTransitions newTransitions trans) newAccSt

-- >>> kleene exampleNFA
-- {  uuid: 0 ,
--    initial: AutState {name = "i", nfaId = 0} ,
--    transitions: fromList [((AutState {name = "0", nfaId = 0},'\NUL'),[AutState {name = "0", nfaId = 0},AutState {name = "e", nfaId = 0}]),((AutState {name = "0", nfaId = 0},'a'),[AutState {name = "0", nfaId = 0}]),((AutState {name = "i", nfaId = 0},'\NUL'),[AutState {name = "0", nfaId = 0},AutState {name = "e", nfaId = 0}])] ,
--    accepting:AutState {name = "e", nfaId = 0}
--  }
