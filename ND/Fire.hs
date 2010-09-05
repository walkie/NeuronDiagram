
module ND.Fire where

import Data.List (findIndex)

import ND.Neuron
import ND.Diagram
import ND.Arg


----------------------
-- Firing Semantics --
----------------------

-- maps neuron names to values
type Map a = [(Name,a)]

-- lookup a neuron in a map
look :: Map a -> N b -> a
look m n = case lookup (name n) m of
             Just a -> a
             _ -> error $ "look: No value for neuron: " ++ show n

-- generate an input map from a graph and list of inputs
inMap :: G a -> [a] -> Map a
inMap = zip . map name . inputs

-- determine the firing state of a neuron in some diagram
evalN :: D a -> N a -> a
evalN (D g i) = evalN' (inMap g i)

-- determine the firing state of a neuron given an input map
evalN' :: Map a -> N a -> a
evalN' m n = case fire n of
               In     -> look m n
               Fire f -> f $ map (evalN' m) (preds n)

-- determine the firing state of all sinks of a neuron diagram
evalD :: D a -> [a]
evalD d = map (evalN d) (sinksD d)

-- firing function for a graph
eval :: G a -> [a] -> [a]
eval g = evalD . D g

-- firing function for each sink in a graph
evals :: G a -> [[a] -> a]
evals g = map e [0 .. length (sinks g) - 1]
  where e i as = eval g as !! i

-- firing function for a particular sink
evalSink :: G a -> Name -> [a] -> a
evalSink g n as = maybe err (eval g as !!) $ findIndex (isNamed n) (sinks g)
  where err = error $ "evalSink: No sink named: " ++ n

-- state of any neuron in the graph
stateOf :: NV a => D a -> Name -> a
stateOf d = evalN d . findNeuron (graph d)

-- construct an argument given a diagram and a neuron name
arg :: D a -> Name -> Arg a
arg d n = Arg n $ evalN d (findNeuron (graph d) n)


-----------------------------
-- Explicit Representation --
-----------------------------

-- explicit representation of the firing semantics
newtype Effects a = Effects [(Rec a, Rec a)]

-- firing semantics
effects :: NV a => G a -> Effects a
effects g = Effects [(r, evalRec r) | r <- allRecs g]
  where snks = map name (sinks g)
        evalRec r = zipWith Arg snks $ eval g (map argVal r)

instance NV a => Show (Effects a) where
  show (Effects es) = unlines (map row es)
    where row (i,o) = show i ++ " -> " ++ show o
