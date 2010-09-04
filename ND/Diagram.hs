
-- This module contains the definitions of neuron graphs and diagrams.
module ND.Diagram where

import Data.List (find,nub)

import ND.Neuron


------------
-- Graphs --
------------

-- neuron graph
newtype G a = G [N a]
  deriving (Eq,Show)

-- all neurons in a graph
neurons :: G a -> [N a]
neurons (G ns) = nub (ns ++ concatMap upstream ns)

-- find a neuron by name in a graph
findNeuron :: G a -> Name -> N a
findNeuron g n = case find (isNamed n) (neurons g) of
                   Just n  -> n
                   Nothing -> error $ "findNeuron: No neuron named: " ++ n

-- all sink neurons
sinks :: G a -> [N a]
sinks (G ns) = ns

-- all source neurons
sources :: G a -> [N a]
sources = filter isEx . neurons

-- all input neurons
inputs :: G a -> [N a]
inputs = filter isIn . neurons


--------------
-- Diagrams --
--------------

-- neuron diagram
data D a = D (G a) [a]
  deriving (Eq,Show)

-- smart constructor for building diagrams
diagram :: [N a] -> [a] -> D a
diagram = D . G

-- the graph underlying a diagram
graph :: D a -> G a
graph (D g _) = g

-- change the inputs of a diagram
withInputs :: D a -> [a] -> D a
withInputs = D . graph

-- input values to a diagram
inVals :: D a -> [a]
inVals (D _ as) = as

-- all neurons in a diagram
neuronsD :: D a -> [N a]
neuronsD = neurons . graph

-- all sinks in a diagram
sinksD :: D a -> [N a]
sinksD = sinks . graph

-- all sources in a diagram
sourcesD :: D a -> [N a]
sourcesD = sources . graph

-- all neuron values
allVals :: NV a => [a]
allVals = [minBound .. maxBound]

-- all inputs of a given length
allIns :: NV a => Int -> [[a]]
allIns 0 = [[]]
allIns n = [h:t | h <- allVals, t <- allIns (n-1)]

allInsFor :: NV a => G a -> [[a]]
allInsFor = allIns . length . inputs

-- all diagrams for a given graph
allDs :: NV a => G a -> [D a]
allDs g = map (D g) (allInsFor g)
