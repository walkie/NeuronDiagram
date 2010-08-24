
-- This module implements the causal semantics and other causal analyses
-- of neuron diagrams.
module ND.Cause where

import Data.Function (on)
import Data.List

import ND.Neuron
import ND.Diagram
import ND.Fire
import ND.BoolRel


----------------------
-- Argument Records --
----------------------

-- argument
data Arg a = Arg Name a
  deriving (Eq,Ord)

-- argument record
type Rec a = [Arg a]

-- alteration of an argument record
type Alt = [Name]

-- argument name
argName :: Arg a -> Name
argName (Arg n _) = n

-- argument value
argVal :: Arg a -> a
argVal (Arg _ a) = a

-- create an argument record for a graph
rec :: G a -> [a] -> Rec a
rec = zipWith Arg . map name . inputs

-- all permutations of an argument record
perms :: NV a => Rec a -> [Rec a]
perms r = delete r $ map (zipWith Arg (map argName r)) (allIns (length r))

-- isolate an alteration given the original argument record and a permutation
alt :: Eq a => Rec a -> Rec a -> Alt
alt o = map argName . filter (flip notElem o)

-- construct an argument given a diagram and a neuron name
arg :: D a -> Name -> Arg a
arg d n = Arg n $ evalN d (findNeuron (graph d) n)

instance NV a => Show (Arg a) where
  show (Arg n a) = n ++ ":" ++ showVal a


------------------------------
-- Counterfactual Reasoning --
------------------------------

-- structured counterfactual dependency
type CFD = DC Name

-- the causes of a single neuron's value
type Cause a = DC (Arg a)

{-
-- basic counterfactual dependency
type BCFD = Dis Name

-- get the basic counterfactual dependencies of a function, given some input
bcfd :: NV a => ([a] -> a) -> Rec a -> BCFD
bcfd f = onList (concatMap singles) . cfd f
  where singles (Con [a]) = [a]
        singles _         = []
-}

-- get the counterfactual dependencies of a function, given some input
cfd :: NV a => ([a] -> a) -> Rec a -> CFD
cfd f r = Dis [Con (alt r p) | p <- perms r, eval r /= eval p]
  where eval = f . map argVal

-- get the minimal counterfactual dependencies of a function, given some input
mcfd :: NV a => ([a] -> a) -> Rec a -> CFD
mcfd f = simplify2 . cfd f

-- convert a CFD to a Cause in the context of some diagram
cfdToCause :: D a -> CFD -> Cause a
cfdToCause d = dist . fromList2 . toList2 . fmap2 (arg d)

-- (structured) counterfactual causes of each sink in a diagram,
-- in terms of inputs
counter :: NV a => D a -> [Cause a]
counter d@(D g i) = map cause (evals g)
  where cause = cfdToCause d . flip mcfd (rec g i)

-- basic (unstructured) counterfactual causes of each sink in a diagram,
-- in terms of inputs
basic :: NV a => D a -> [Cause a]
basic = filter ((==1) . length . toList) . counter


-----------------
-- Causal Flow --
-----------------

-- immediate (local) counterfactual cause of a neuron
local :: NV a => D a -> N a -> Cause a
local d n | isAct n   = fromList2 [[arg d (name n)]] -- stop recursing on action
          | otherwise = cfdToCause d (law (fire n))
  where law (Fire f) = mcfd f [arg d (name p) | p <- preds n]
        law In       = fromList2 [[]]

-- ultimate cause of a neuron by causal flow
flow :: (NV a, Ord a) => D a -> N a -> Cause a
flow d n = (simplify2 . flatten) (fmap2 expand (local d n))
  where expand a@(Arg m _) | m == name n = fromList2 [[a]]
                           | otherwise   = flow d (findNeuron (graph d) m)

-- causal semantics
sem :: (NV a, Ord a) => D a -> [Cause a]
sem d = map (flow d) (sinksD d)
