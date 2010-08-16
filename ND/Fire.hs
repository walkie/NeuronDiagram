
module ND.Fire where

import ND.Neuron
import ND.Diagram


----------------------
-- Firing Semantics --
----------------------

-- maps neuron names to values
type Map a = [(Name,a)]

-- lookup a neuron in a map
look :: Map a -> N b -> a
look m n = case lookup (name n) m of
             Just a -> a
             _ -> error $ "No value for neuron: " ++ show n

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
