{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module Neuron where

import Data.Function (on)
import Data.List (intersperse,nub)

import System (system)
import Prelude hiding (writeFile)
import System.IO.UTF8 (writeFile)

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
data N a = forall t. NT t a => Name :> t a

infix 5 :>

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

instance NT N a where
  fire  (_ :> t) = fire t
  kind  (_ :> t) = kind t
  preds (_ :> t) = preds t
  nodeAttrs (_ :> t) = nodeAttrs t
  edgeAttrs (_ :> t) = edgeAttrs t

-- neuron values
class NV a where
  showVal  :: a -> String
  valAttrs :: a -> [Attr]

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


-- neuron name
name :: N a -> Name
name (n :> _) = n

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


------------------
-- Core Neurons --
------------------

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
data K t a = K (t a) Kind
  deriving (Eq,Show)

instance NT t a => NT (K t) a where
  fire  (K t _) = fire t
  kind  (K _ k) = k
  preds (K t _) = preds t


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


-------------------------
-- Graphs and Diagrams --
-------------------------

-- neuron graph
newtype G a = G [N a]
  deriving (Eq,Show)

-- neuron diagram
data D a = D (G a) [a]
  deriving (Eq,Show)

-- all neurons in a graph
neurons :: G a -> [N a]
neurons (G ns) = nub (ns ++ concatMap upstream ns)

-- all sink neurons
sinks :: G a -> [N a]
sinks (G ns) = ns

-- all source neurons
sources :: G a -> [N a]
sources = filter isEx . neurons

-- the graph underlying a diagram
graph :: D a -> G a
graph (D g _) = g

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

-- all diagrams for a given graph
allDs :: (Bounded a, Enum a) => G a -> [D a]
allDs g = map (D g) (allIns (numIns g))


-------------------------
-- Dealing with Inputs --
-------------------------

-- maps neuron names to values
type Map a = [(Name,a)]

-- lookup a neuron in a map
look :: Map a -> N b -> a
look m n = case lookup (name n) m of
             Just a -> a
             _ -> error $ "No value for neuron: " ++ show n

-- names of all input nodes
inNames :: G a -> [Name]
inNames = map name . filter isIn . neurons

-- names of all output nodes
outNames :: G a -> [Name]
outNames = map name . sinks

-- number of input nodes
numIns :: G a -> Int
numIns = length . inNames

-- generate an input map from a graph and list of inputs
inMap :: G a -> [a] -> Map a
inMap = zip . inNames

-- all inputs of a given length
allIns :: (Bounded a, Enum a) => Int -> [[a]]
allIns 0 = [[]]
allIns n = [h:t | h <- [minBound .. maxBound], t <- allIns (n-1)]


----------------------
-- Firing Semantics --
----------------------

-- determine the firing state of a neuron in some diagram
evalN :: D a -> N a -> a
evalN (D g i) = evalN' (inMap g i)

-- determine the firing state of a neuron given an input map
evalN' :: Map a -> N a -> a
evalN' m n = case fire n of
               In     -> look m n
               Fire f -> f $ map (evalN' m) (preds n)

-- determine the firing state of all sinks of a neuron diagram
evalD :: D a -> [a]
evalD d = map (evalN d) (sinksD d)


---------------------
-- Causal Analyses --
---------------------

{-
type Cond a = Arg a
newtype Cause a = Cause [Cond a]    -- conjunction of conditions
newtype Causes a = Causes [Cause a] -- disjunction of causes
-}

-- counterfactual causes

data Arg a = Arg Name a
  deriving Eq

instance Show a => Show (Arg a) where
  show (Arg n a) = show



--------------------
-- GraphViz Stuff --
--------------------

-- we use ids to refer to neurons so that neuron names are not limited
-- to valid graphviz identifiers.
type Id = String

-- a string of graphviz code
type GV = String

-- construct an id map for a graph
idMap :: G a -> Map Id
idMap g = zip (map name (neurons g)) (map show [1..])

-- print an attribute
gvA :: Attr -> GV
gvA (n,v) = n ++ "=" ++ v

-- print a list of attributes
gvAs :: [Attr] -> GV
gvAs as = "[" ++ concat (intersperse "," (map gvA as)) ++ "]"

-- print an edge
gvE :: Map Id -> N a -> N a -> [Attr] -> GV
gvE ids n m as = look ids m ++ "->" ++ look ids n ++ gvAs as ++ ";"

-- print edges
gvEs :: Map Id -> N a -> GV
gvEs ids n = concat $ zipWith (gvE ids n) (preds n) (edgeAttrs n)

-- print a node
gvN :: NV a => Map Id -> Maybe a -> N a -> GV
gvN ids v n = look ids n ++ gvAs (label : width : nodeAttrs n ++ valAttrs v) ++ ";"
  where label = ("label", name n ++ if isLaw n then "§" else "")
        width = ("width", "1")

-- helper function for printing graphs and diagrams
gvG' :: NV a => Map Id -> G a -> (N a -> Maybe a) -> GV
gvG' ids g f = "digraph{rankdir=LR;margin=0.05;" ++ ns ++ "}"
  where ns = concat [gvN ids (f n) n ++ gvEs ids n | n <- neurons g]

-- print a graph
gvG :: NV a => G a -> GV
gvG g = gvG' (idMap g) g (const Nothing)

-- print a diagram
gvD :: NV a => D a -> GV
gvD (D g i) = gvG' (idMap g) g (Just . evalN' (inMap g i))

write :: FilePath -> (a -> GV) -> a -> IO ()
write p f a = writeFile p (f a)

view :: (a -> GV) -> a -> IO ()
view f a = write "view.dot" f a >> system "open view.dot" >> return ()

-- write a graphviz file for a graph
writeG :: NV a => FilePath -> G a -> IO ()
writeG = flip write gvG

-- write a graphviz file for a diagram
writeD :: NV a => FilePath -> D a -> IO ()
writeD = flip write gvD

-- pop up a graphviz view of a graph
viewG :: NV a => G a -> IO ()
viewG = view gvG

-- pop up a graphviz view of a diagram
viewD :: NV a => D a -> IO ()
viewD = view gvD


--------------
-- Examples --
--------------

-- desert traveler problem (preemption)
desert :: G Bool
desert = G [dead]
  where poison = "Poison" :> Input
        poke   = "Poke"   :> Input
        drink  = "Drink"  :> Stim [poison] `Inhib` [poke]
        thirst = "Thirst" :> Stim [poke]
        dead   = "Dead"   :> Stim [drink,thirst]

desert' = D desert [True,True]
