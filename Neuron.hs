{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses,
             NoMonomorphismRestriction #-}

module Neuron where

----------------------
-- Neurons diagrams --
----------------------

type Name = String
type Fire = Bool           -- basic binary firing

data Attr = Name := String -- graphviz attribute

class (Show t, GraphElem t, Value a) =>
      EdgeType t a where
  evalE :: t -> a -> a          -- t -> source -> target

class (Show t, GraphElem t, Value a) =>
      NeuronType t a where
  evalN :: t -> [a] -> [a] -> a -- t -> stims -> inhibs -> result

-- an edge has a function and a source neuron
data Edge a = forall t. EdgeType t a => 
              Edge t (Neuron a)

-- first edges are stimulating edges into this node,
-- second are inhibiting edges into this node
data Neuron a = forall t. NeuronType t a =>
                Neuron t Name [Edge a] [Edge a]

eval :: Value a => Neuron a -> a
eval (Neuron t _ ss is) = evalN t (map eval' ss) (map eval' is)
  where eval' (Edge t s) = evalE t (eval s)

-----------------------------
-- Basic neurons and edges --
-----------------------------

--
-- A simple value-passing edge.
--

data Id = Id deriving (Eq, Show)
instance GraphElem Id where
  attr _ = []
instance Value a => EdgeType Id a where
  evalE _ = id

--
-- An exogenous neuron.
--

data Exo a = Exo a deriving (Eq, Show)
instance GraphElem (Exo a) where
  attr _ = []
instance (Show a, Value a) => NeuronType (Exo a) a where
  evalN (Exo a) _ _ = a

--
-- A regular neuron.
--

data Plain = Plain deriving (Eq, Show)
instance GraphElem Plain where
  attr _ = []
instance Value a => NeuronType Plain a where
  evalN _ ss is = times (sumV ss) (neg (sumV is))

--
-- A double-thick neuron.
-- Fires if stimulated by two nonzero edges (and inhibited by none).
--

data Thick = Thick deriving (Eq, Show)
instance GraphElem Thick where
  attr _ = ["penwidth" := "2.0"]
instance NeuronType Thick Bool where
  evalN _ ss is | nfire ss > 2 && nfire is == 0 = one
                | otherwise                     = zero

nfire :: [Fire] -> Int
nfire = length . filter id

--
-- A neuron that fires if it is stimulated more than inhibited.
--   (TODO: Probably not generalized right to non-booleans. Needs compare...)
--

data Balance = Balance deriving (Eq, Show)
instance GraphElem Balance where
  attr _ = ["style" := "dotted"]  -- to match Hitchcock '09
instance NeuronType Balance Bool where
  evalN _ ss is | nfire ss > nfire is = one
                | otherwise           = zero


-- Smart constructors

edge :: Value a => Neuron a -> Edge a
edge = Edge Id

neuron :: (Show a, Value a, NeuronType t a) =>
          t -> Name -> [Neuron a] -> [Neuron a] -> Neuron a
neuron t n ss is = Neuron t n (map edge ss) (map edge is)

source :: (Show a, Value a) => Name -> a -> Neuron a
source n a = Neuron (Exo a) n [] []

plain   = neuron Plain
thick   = neuron Thick
balance = neuron Balance

------------
-- Values --
------------

class Eq a => Value a where
  zero  :: a
  one   :: a
  plus  :: a -> a -> a
  times :: a -> a -> a
  neg   :: a -> a

instance Value Bool where
  zero  = False
  one   = True
  plus  = (||)
  times = (&&)
  neg   = not

sumV :: Value a => [a] -> a
sumV = foldr plus zero

productV :: Value a => [a] -> a
productV = foldr times one

--------------------
-- Show instances --
--------------------

instance Show (Edge a) where
  show (Edge t s) = "Edge (" ++ show t ++ ") " ++ show s

instance Show (Neuron a) where
  show (Neuron t n ss is) = "Neuron (" ++ show t ++ ") " ++ show n ++ " " ++ show ss ++ " " ++ show is

--------------------
-- GraphViz stuff --
--------------------

-- There is a Graphviz library on Hackage...
-- Could switch to that, but probably overkill.

class GraphElem a where
  attr :: a -> [Attr]

instance GraphElem Bool where
  attr True = ["style" := "filled", "fillcolor" := "gray"]
  attr _    = []

--graphvizN :: GraphElem a => Neuron a -> String
--graphvizN (Neuron t n _ _) = n ++ show 


--------------
-- Examples --
--------------

poison = source "poison" True
poke   = source "poke"   True
drink  = plain  "drink"  [poison]       [poke]
thirst = plain  "thirst" [poke]         []
dead   = plain  "dead"   [drink,thirst] []
