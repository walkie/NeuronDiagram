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
data N a = forall d. Desc d a => Name :<- d a

infix 5 :<-

-- neuron type
class Desc d a where
  fire  :: d a -> Fire a -- firing function
  preds :: d a -> [N a]  -- immediate predecessors
  kind  :: d a -> Kind   -- neuron kind
  nodeAttrs :: d a -> [Attr]   -- graphviz node attributes
  edgeAttrs :: d a -> [[Attr]] -- graphviz edge attributes
  
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

-- helper function for constructing arrowhead attributes
arrowhead :: String -> [Attr]
arrowhead s = [("arrowhead",s)]

-- helper function for constructing shape attributes
shape :: String -> [Attr]
shape s = [("shape",s)]

instance Desc N a where
  fire  (_ :<- d) = fire d
  kind  (_ :<- d) = kind d
  preds (_ :<- d) = preds d
  nodeAttrs (_ :<- d) = nodeAttrs d
  edgeAttrs (_ :<- d) = edgeAttrs d

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
