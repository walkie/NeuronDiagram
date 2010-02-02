{-# LANGUAGE ExistentialQuantification #-}

module Neuron where

type Id   = Int
type Fire = Bool
type Solution = [(Id,Fire)]

data EdgeType = Stimulate | Inhibit

data Edge e = 
  Edge {
    edgeType :: EdgeType,
    edgeData :: e,
    edgeFrom :: Id
  }

data Neuron n e = forall t. NeuronType t =>
  Neuron {
    neuronId   :: Id,
    neuronType :: t,
    neuronData :: n,
    edgesTo    :: [Edge e]
  }

type Diagram n e = [Neuron n e]

class GraphElem t => NeuronType t where
  graphvizAttrs :: t -> [Attr]
  --fire :: [Input] -> t -> Fire

sources :: Diagram n e -> [Neuron n e]
sources = filter (null . edgesTo)

sourceIds :: Diagram n e -> [Id]
sourceIds = map neuronId . sources

removeById :: (a -> Id) -> [Id] -> [a] -> [a]
removeById f ids as = [a | a <- as, f a `notElem` ids]
--removeById f ids = filter (flip notElem ids . f)

removeNeurons :: [Id] -> [Neuron n e] -> [Neuron n e]
removeNeurons = removeById neuronId

removeEdgesFrom :: [Id] -> [Edge e] -> [Edge e]
removeEdgesFrom = removeById edgeFrom

fireOrder :: Diagram n e -> [Id]
fireOrder [] = []
fireOrder ns = ids ++ fireOrder ns'
  where ids = sourceIds ns
        ns' = [ Neuron i t d (removeEdgesFrom ids es)
              | Neuron i t d es <- removeNeurons ids ns ]
-- below is a bit more extensible way of writing this, but GHC can't handle
-- record notation with ExistentialTypes yet...
--      ns' = [ n { edgesTo = removeEdgesFrom ids (edgesTo n) } 
--            | n <- removeNeurons ids ns ]

data Plain = Plain

instance GraphElem Plain where
  graphvizAttrs Plain = []

instance NeuronType Plain

-------------
-- Example --
-------------

plain i = Neuron i Plain
stim    = Edge Stimulate ""
inhib   = Edge Inhibit   ""

poison = plain 1 "poison" []
poke   = plain 2 "poke"   []
drink  = plain 3 "drink"  [stim 1,inhib 2]
thirst = plain 4 "thirst" [stim 2]
dead   = plain 5 "dead"   [stim 3,stim 4]

desert = [poison,poke,drink,thirst,dead]
solution = [(1,True),(2,True),(3,False),(4,True),(5,True)] :: Solution

--------------------
-- Graphviz stuff --
--------------------

type Name = String
data Attr = Name := String

class GraphvizElem a where
  showG 
  graphvizAttrs _ = []

instance GraphElem EdgeType where
  graphvizAttrs Stimulate = []
  graphvizAttrs Inhibit   = ["arrowhead" := "dot"]
  
instance Show Attr where
  show (n := v) = n ++ "=" ++ v

-- This currently ignores node and edge data.  Probably should be adapted to
-- use these instead of ids.
graphviz :: (Show n, Show e) => Diagram n e -> Solution -> String
graphviz d s = "digraph{rankdir=LR;node[fixedsize=true,width=1];" -- doesn't scale with labels...
               ++ concatMap neuron d ++ concatMap edges d ++ "}"
  where neuron (Neuron i t d _ ) = show i ++ show ("label" := show d : graphvizAttrs t ++ fired i) ++ ";"
        edges  (Neuron i _ _ es) = concatMap (edge i) es
        edge d (Edge t _ s)   = show s ++ "->" ++ show d ++ show (graphvizAttrs t) ++ ";"
        fired i = case lookup i s of
                    Just True  -> ["style" := "filled", "fillcolor" := "gray"]
                    Just False -> []
                    Nothing    -> error ("no solution for node: " ++ show i)
