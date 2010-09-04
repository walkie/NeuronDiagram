{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses #-}

-- This module contains definitions which are used by every other module,
-- including type definitions for neurons, neurons types, and neuron values.
module ND.Neuron where

import Data.Function (on)
import Data.List     (nub)


-------------
-- Neurons --
-------------

-- neuron name
type Name = String

-- graphviz attribute
type Attr = (Name,String)

-- firing function
data Fire a = In | Fire ([a] -> a)

-- neuron kind
data Kind = Act | Law
  deriving (Eq,Show)

-- neuron
data N a = forall t. NT t a => Name :<- t a

infix 5 :<-

-- neuron type
class NT t a where
  fire  :: t a -> Fire a -- firing function
  preds :: t a -> [N a]  -- immediate predecessors
  kind  :: t a -> Kind   -- neuron kind
  nodeAttrs :: t a -> [Attr]   -- graphviz node attributes
  edgeAttrs :: t a -> [[Attr]] -- graphviz edge attributes
  
  -- defaults
  kind _  = Law
  preds _ = []
  nodeAttrs _ = [] 
  edgeAttrs t = [[] | _ <- preds t]

-- neuron values
class (Bounded a, Enum a, Eq a, Show a) => NV a where
  --showVal  :: a -> String
  valAttrs :: a -> [Attr]
  --labelFun :: a -> String -> String

-- helper function for constructing attributes that fill a graphviz
-- node with color
fillWith :: String -> [Attr]
fillWith c = [("style","filled"),("fillcolor",c)]

instance NT N a where
  fire  (_ :<- t) = fire t
  kind  (_ :<- t) = kind t
  preds (_ :<- t) = preds t
  nodeAttrs (_ :<- t) = nodeAttrs t
  edgeAttrs (_ :<- t) = edgeAttrs t

-- neuron name
name :: N a -> Name
name (n :<- _) = n

-- is the neuron named n?
isNamed :: Name -> N a -> Bool
isNamed n = (n==) . name

-- is action?
isAct :: N a -> Bool
isAct = (Act ==) . kind

-- is law?
isLaw :: N a -> Bool
isLaw = (Law ==) . kind

-- is this an input neuron?
isIn :: N a -> Bool
isIn n = case fire n of
           In -> True
           _  -> False

-- is exogenous?
isEx :: N a -> Bool
isEx = null . preds

-- is endogenous?
isEn :: N a -> Bool
isEn = not . isEx

-- all upstream neurons
upstream :: N a -> [N a]
upstream n = (nub . concat) [p : upstream p | p <- preds n]

-- instances
instance Eq (N a) where
  (==) = (==) `on` name
instance Show (N a) where
  show n = name n ++ if isEx n then "" else show (preds n)
