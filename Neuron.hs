{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses,
             NoMonomorphismRestriction #-}

module Neuron where

import Data.List (nubBy,partition)
import System    (system)


----------------------
-- Neurons diagrams --
----------------------

type Name = String

data Input a   = In InputType a deriving (Eq, Show)
data InputType = S | I          deriving (Eq, Show) -- stimulate/inhibit

class (Show t, GraphElem t, Value a) =>
      EdgeType t a where
  evalE :: t -> a -> Input a

class (Show t, GraphElem t, Value a) =>
      NeuronType t a where
  evalN :: t -> [Input a] -> a

data Edge a = forall t. EdgeType t a => 
              Edge t (Neuron a)

data Neuron a = forall t. NeuronType t a =>
                Neuron t Name [Edge a]

eval :: Value a => Neuron a -> a
eval (Neuron t _ es) = evalN t (map eval' es)
  where eval' (Edge t s) = evalE t (eval s)


----------------------
-- Helper functions --
----------------------

name :: Neuron a -> Name
name (Neuron _ n _) = n

-- immediate predecessors
pointsTo :: Neuron a -> [Neuron a]
pointsTo (Neuron _ _ es) = [n | Edge _ n <- es]

-- all predecessors
flowsTo :: Neuron a -> [Neuron a]
flowsTo n = nubBy (same name) (n : concatMap flowsTo (pointsTo n))

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

isS :: Input a -> Bool
isS (In S _) = True
isS _        = False

isI :: Input a -> Bool
isI (In I _) = True
isI _        = False

val :: Input a -> a
val (In _ a) = a

partIns :: [Input a] -> ([a],[a])
partIns es = (map val ss, map val is)
  where (ss,is) = partition isS es

nfire :: [Bool] -> Int
nfire = length . filter id


-----------------------------
-- Basic neurons and edges --
-----------------------------

-- Basic stimulating edge
data Stim = Stim deriving (Eq, Show)
instance Value a => EdgeType Stim a where
  evalE _ = In S

-- Basic inhibiting edge
data Inhib = Inhib deriving (Eq, Show)
instance Value a => EdgeType Inhib a where
  evalE _ = In I

-- Exogenous neuron
data Exo a = Exo a deriving (Eq, Show)
instance (Show a, Value a) => NeuronType (Exo a) a where
  evalN (Exo a) _ = a

-- Regular (endogenous) neuron
data Endo = Endo deriving (Eq, Show)
instance Value a => NeuronType Endo a where
  evalN _ es = times (sumV ss) (neg (sumV is))
    where (ss,is) = partIns es

-- Thick neuron - fires if stimulated by two edges (and inhibited by none)
data Thick = Thick deriving (Eq, Show)
instance NeuronType Thick Bool where
  evalN _ es | nfire ss > 2 && nfire is == 0 = True
             | otherwise                     = False
    where (ss,is) = partIns es

-- Neuron that fires if it is stimulated more than inhibited.
data Elect = Elect deriving (Eq, Show)
instance NeuronType Elect Bool where
  evalN _ es | nfire ss > nfire is = one
             | otherwise           = zero
    where (ss,is) = partIns es


------------------------
-- Smart constructors --
------------------------

stim :: Value a => Neuron a -> Edge a
stim = Edge Stim

inhib :: Value a => Neuron a -> Edge a
inhib = Edge Inhib

neuron :: (Show a, Value a, NeuronType t a) =>
          t -> Name -> [Neuron a] -> [Neuron a] -> Neuron a
neuron t n ss is = Neuron t n (map stim ss ++ map inhib is)

exo :: (Show a, Value a) => Name -> a -> Neuron a
exo n a = Neuron (Exo a) n []

endo  = neuron Endo
thick = neuron Thick
elect = neuron Elect


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
-- Show instances -- -- These are really ugly...
--------------------

instance Show (Edge a) where
  show (Edge t s) = "Edge (" ++ show t ++ ") (" ++ show s ++ ")"

instance Show (Neuron a) where
  show (Neuron t n es) = "Neuron (" ++ show t ++ ") " ++ show n ++ " " ++ show es

instance Show Attr where
  show (n := v) = n ++ "=" ++ v


--------------------
-- GraphViz stuff --
--------------------

-- There is a Graphviz library on Hackage...
-- Could switch to that, but probably overkill.

-- Graphviz attribute
data Attr = Name := String

class GraphElem a where
  attr :: a -> [Attr]

-- values
instance GraphElem Bool where
  attr True = ["style" := "filled", "fillcolor" := "gray"]
  attr _    = []

-- edges
instance GraphElem Stim where
  attr _ = []
instance GraphElem Inhib where
  attr _ = ["arrowhead" := "dot"]

-- neurons
instance GraphElem (Exo a) where
  attr _ = []
instance GraphElem Endo where
  attr _ = []
instance GraphElem Thick where
  attr _ = ["penwidth" := "2.0"]
instance GraphElem Elect where
  attr _ = ["style" := "dotted"]  -- to match Hitchcock '09

-- edges currently can't vary their appearance depending on their value
graphvizE :: GraphElem a => Name -> Edge a -> String
graphvizE d (Edge t s) = name s ++ "->" ++ d ++ show (attr t) ++ ";"

graphvizN :: GraphElem a => Neuron a -> String
graphvizN n@(Neuron t i es) = i ++ show (attr (eval n) ++ attr t) ++ ";"
                              ++ concatMap (graphvizE i) es

graphviz :: GraphElem a => Neuron a -> String
graphviz n = "digraph{rankdir=LR;node[fixedsize=true,width=1];" -- doesn't scale with labels...
             ++ concatMap graphvizN (flowsTo n) ++ "}"

view :: GraphElem a => Neuron a -> IO ()
view n = writeFile "view.dot" (graphviz n) >> system "open view.dot" >> return ()


--------------
-- Examples --
--------------

desert x y = dead
  where poison = exo  "poison" x
        poke   = exo  "poke"   y
        drink  = endo "drink"  [poison]       [poke]
        thirst = endo "thirst" [poke]         []
        dead   = endo "dead"   [drink,thirst] []

boulder x = live
  where boulder = exo  "boulder" x
        alive   = exo  "alive"   True
        duck    = endo "duck"    [boulder] []
        crush   = endo "crush"   [boulder] [duck]
        live    = endo "live"    [alive]   [crush]
