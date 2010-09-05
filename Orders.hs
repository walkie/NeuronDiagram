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
  
instance NT Stim Order where
  fire _ = Fire process
  -- rest same as Stim Bool
  preds (Stim ps) = ps
  
instance NT t Order => NT (Inhib t) Order where
  fire (Inhib t is) = Fire $ fireD t (length is) override isOverridden
  -- rest same as Inhib Bool
  kind  (Inhib t _ ) = kind t
  preds (Inhib t is) = preds t ++ is
  edgeAttrs (Inhib t _) = edgeAttrs t ++ [[("arrowhead","dot")]]
  nodeAttrs (Inhib t _) = nodeAttrs t
