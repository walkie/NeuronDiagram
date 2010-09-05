{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Orders where

import ND

--------------------
-- Orders Example --
--------------------

-- based on example beginning on SE p. 874

-- private will charge if either superior orders him to
major = diagram [pvt] [False,True]
  where gen = "Gen" :<- Input
        maj = "Maj" :<- Input
        pvt = "Pvt" :<- Stim [gen,maj]

major' = diagram ["Pvt" :<- Stim ["Gen" :<- Input, "Maj" :<- Input]] [False,True]

-- same as "orders", but both officers issue the order
both = major `withInputs` [True,True]

-- if given, general's charge order trumps major's charge order
-- same firing semantics, but different causal semantics
trump = diagram [pvt] [True,True]
  where gen  = "Gen"  :<- Input
        maj  = "Maj"  :<- Input
        majE = "MajE" :<- Stim [maj] `Inhib` [gen]
        pvt  = "Pvt"  :<- Stim [gen,majE]

orcs' = diagram [dead] [True]
  where orcs = "Orcs" :<- Input
        gen  = "Gen"  :<- Stim [orcs]
        dead = "Dead" :<- Stim [orcs] `Inhib`  [gen]

orcs = diagram [dead] [True]
  where orcs = "Orcs" :<- Input
        gen  = "Gen"  :<- Stim [orcs] `IsKind` Act
        dead = "Dead" :<- Stim [orcs] `Inhib` [gen]

-- incorporate retreat orders
-- exactly the same as charge2 except the type annotation!
retreat = diagram [pvt] [Charge,Retreat]
  where gen  = "Gen"  :<- Input
        maj  = "Maj"  :<- Input
        majE = "MajE" :<- Stim [maj] `Inhib` [gen]
        pvt  = "Pvt"  :<- Stim [gen,majE]

-- adding retreat orders
data Order = None | Charge | Retreat
  deriving (Bounded,Enum,Eq,Ord,Show)

-- for Stim
-- if all orders agree (same orders or None), execute that Order, otherwise do None
process :: [Order] -> Order
process os | all (==None) os      = None
           | allNoneOr Charge  os = Charge
           | allNoneOr Retreat os = Retreat
           | otherwise            = None
  where allNoneOr o = all (flip elem [None,o])

-- for Inhib
-- if any order is not none, order is overridden
isOverridden :: [Order] -> Bool
isOverridden = any (/=None)

-- for Inhib
-- if order is overridden, return None, otherwise return the order
override :: Order -> Bool -> Order
override _ True = None
override o _    = o

instance NV Order where
  --showVal  = show
  valAttrs None    = []
  valAttrs Charge  = fillWith "palegreen"
  valAttrs Retreat = fillWith "pink"
  
instance Desc Stim Order where
  fire _ = Fire process
  -- rest same as Stim Bool
  preds (Stim ps) = ps
  
instance Desc d Order => Desc (Inhib d) Order where
  fire (Inhib d is) = fireDec d (length is) override isOverridden
  -- rest same as Inhib Bool
  kind  (Inhib d _ ) = kind d
  preds (Inhib d is) = preds d ++ is
  edgeAttrs (Inhib d is) = edgeAttrs d ++ replicate (length is) (arrowhead "dot")
  nodeAttrs (Inhib d _ ) = nodeAttrs d
