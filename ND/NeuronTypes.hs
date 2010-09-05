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

instance Desc Input a where
  fire _ = In
  kind _ = Act


-- constant neurons
data Const a = Const a
  deriving (Eq,Show)

instance Desc Const a where
  fire (Const a) = Fire (const a)


-- standard neurons
data Stim a = Stim [N a]
  deriving (Eq,Show)

instance Desc Stim Bool where
  fire  _         = Fire or
  preds (Stim ps) = ps


-- xor neuron
data XOR a = XOR [N a]

instance Desc XOR Bool where
  fire  _        = Fire ((==1) . count)
  preds (XOR ps) = ps
  nodeAttrs _    = [("shape","diamond")]


-- thick neurons
data Thick a = Thick Int [N a]

instance Desc Thick Bool where
  fire  (Thick n _)  = Fire ((>=n) . count)
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

instance Desc t a => Desc (IsKind t) a where
  fire  (IsKind t _) = fire t
  kind  (IsKind _ k) = k
  preds (IsKind t _) = preds t
  edgeAttrs (IsKind t _) = edgeAttrs t
  nodeAttrs (IsKind t _) = nodeAttrs t

-- inhibiting edges decorator
data Inhib t a = Inhib (t a) [N a]
  deriving (Eq,Show)

instance Desc t Bool => Desc (Inhib t) Bool where
  fire  (Inhib t is) = fireDec t (length is) (&&) (all not)
  kind  (Inhib t _ ) = kind t
  preds (Inhib t is) = preds t ++ is
  edgeAttrs (Inhib t is) = edgeAttrs t ++ replicate (length is) (arrowhead "dot")
  nodeAttrs (Inhib t _ ) = nodeAttrs t


-- helper function for defining decorators that modify the firing function
--   t: neuron type being decorated
--   n: number of inputs this neuron consumes
--   c: function for combining results of (fire t) and g
--   d: function to execute on this decorator's inputs
--   as: input to firing function
fireDec :: Desc t a => t a -> Int -> (a -> b -> a) -> ([a] -> b) -> Fire a
fireDec t n c d = Fire $ \as -> 
    let (bs,cs) = splitAt (length (preds t)) as in
    case fire t of
       Fire f -> c (f bs) (d (take n cs))
       _ -> error "Cannot modify input neuron function!"


----------------------------
-- Neuron Value Instances --
----------------------------

instance Bounded a => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just maxBound

instance Enum a => Enum (Maybe a) where
  toEnum 0 = Nothing
  toEnum i = Just (toEnum (i-1))
  fromEnum Nothing  = 0
  fromEnum (Just a) = fromEnum a + 1
  
instance NV a => NV (Maybe a) where
  --showVal  (Just a) = showVal a
  --showVal  Nothing  = "N/A"
  valAttrs (Just a) = valAttrs a
  valAttrs Nothing  = [("style","dashed")]

instance NV Bool where
  --showVal  True  = "T"
  --showVal  False = "F"
  valAttrs True  = fillWith "gray"
  valAttrs False = []
