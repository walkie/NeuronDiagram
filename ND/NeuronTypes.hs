{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses #-}

-- This module contains definitions for several different neuron types.
-- It also includes instances for a few basic neuron values.
module ND.NeuronTypes where

import ND.Neuron


-----------------------
-- Core Neuron Types --
-----------------------

-- input neurons
data Input a = Input
  deriving (Eq,Show)

instance NT Input a where
  fire _ = In
  kind _ = Act


-- constant neurons
data Const a = Const a
  deriving (Eq,Show)

instance NT Const a where
  fire (Const a) = Fire (const a)


-- standard neurons
data Stim a = Stim [N a]
  deriving (Eq,Show)

instance NT Stim Bool where
  fire  _        = Fire or
  preds (Stim ps) = ps


-- xor neuron
data XOR a = XOR [N a]

instance NT XOR Bool where
  fire  _        = Fire ((==1) . count)
  preds (XOR ps) = ps
  nodeAttrs _    = [("shape","diamond")]


-- thick neurons
data Thick a = Thick Int [N a]

instance NT Thick Bool where
  fire  (Thick n _)  = Fire ((>n) . count)
  preds (Thick _ ps) = ps
  nodeAttrs _        = [("penwidth","3")]


-- number of true values
count :: [Bool] -> Int
count = length . filter id


-----------------------
-- Neuron Decorators --
-----------------------

-- explicit kind decorator
data IsKind t a = IsKind (t a) Kind
  deriving (Eq,Show)

instance NT t a => NT (IsKind t) a where
  fire  (IsKind t _) = fire t
  kind  (IsKind _ k) = k
  preds (IsKind t _) = preds t


-- inhibiting edges decorator
data Inhib t a = Inhib (t a) [N a]
  deriving (Eq,Show)

instance NT t Bool => NT (Inhib t) Bool where
  fire  (Inhib t is) = Fire $ fireD t (length is) (&&) (all not)
  kind  (Inhib t _ ) = kind t
  preds (Inhib t is) = preds t ++ is
  edgeAttrs (Inhib t is) = edgeAttrs t ++ [[("arrowhead","dot")]]


-- helper function for defining decorators that modify the firing function
--   t: neuron type being decorated
--   n: number of inputs this neuron consumes
--   c: function for combining results of (fire t) and g
--   d: function to execute on this decorator's inputs
--   as: input to firing function
fireD :: NT t a => t a -> Int -> (a -> a -> a) -> ([a] -> a) -> [a] -> a
fireD t n c d as = case fire t of
                     Fire f -> c (f bs) (d (take n cs))
                     _ -> error "Cannot modify input neuron function!"
  where (bs,cs) = splitAt (length (preds t)) as


----------------------------
-- Neuron Value Instances --
----------------------------

instance NV a => NV (Maybe a) where
  showVal  (Just a) = showVal a
  showVal  Nothing  = "N/A"
  valAttrs (Just a) = valAttrs a
  valAttrs Nothing  = [("style","dashed")]

instance NV Bool where
  showVal  True  = "T"
  showVal  False = "F"
  valAttrs True  = [("style","filled"),("fillcolor","gray")]
  valAttrs False = []
