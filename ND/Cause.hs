
-- This module implements the causal semantics and other causal analyses
-- of neuron diagrams.
module ND.Cause where

import Data.Function (on)
import Data.List

import ND.Neuron
import ND.Diagram
import ND.Arg
import ND.Fire
import ND.BoolRel


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
local d n | isAct n   = fromList2 [[arg d (name n)]] -- actions are their own cause
          | otherwise = cfdToCause d (law (fire n))  -- laws are determined through CF reasoning
  where law (Fire f) = mcfd f [arg d (name p) | p <- preds n]
        law In       = fromList2 [[]]

-- ultimate cause of a neuron by causal flow
flow :: (NV a, Ord a) => D a -> N a -> Cause a
flow d n = (simplify2 . flatten) (fmap2 expand (local d n))
  where expand a@(Arg m _) | m == name n = fromList2 [[a]]
                           | otherwise   = flow d (findNeuron (graph d) m)

-- causal semantics
csem :: (NV a, Ord a) => D a -> [Cause a]
csem d = map (flow d) (sinksD d)

-- print the causal semantics in a nice way
causes :: (NV a, Ord a) => D a -> IO ()
causes d = mapM_ putStr $ zipWith cause (csem d) (sinksD d)
  where cause c n = show c ++ " ==> " ++ show (Arg (name n) (evalN d n)) ++ "\n"
