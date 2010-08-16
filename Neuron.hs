{-# LANGUAGE ExistentialQuantification, PatternGuards #-}

module Neuron where

import Control.Monad (mapM_)
import Data.Function (on)
import Data.List
import System (system)

import Prelude hiding (writeFile)
import System.IO.UTF8 (writeFile)

-------------
-- Neurons --
-------------

type Name = String

-- neuron
data N = forall d. ND d => Name :> d

data Fire = Input | Fire ([Bool] -> Bool)

-- neuron definition
class ND d where
  stims'  :: d -> [N]
  inhibs' :: d -> [N]
  isLaw'  :: d -> Bool
  fire'   :: d -> Fire
  attrs'  :: d -> [Attr]

stims  (_ :> d) = stims'  d
inhibs (_ :> d) = inhibs' d
isLaw  (_ :> d) = isLaw'  d
fire   (_ :> d) = fire'   d
attrs  (_ :> d) = attrs'  d

-- name of neuron
name :: N -> Name
name (n :> _) = n

-- immediate predecessors
preds :: N -> [N]
preds n = stims n ++ inhibs n

-- all predecessors
upstream :: N -> [N]
upstream n = (nub . concat) [p : upstream p | p <- preds n]

-- is action?
isAct :: N -> Bool
isAct = not . isLaw

-- is exogenous?
isEx :: N -> Bool
isEx = null . preds

-- is endogenous?
isEn :: N -> Bool
isEn = not . isEx

-- is this an input node?
isIn :: N -> Bool
isIn n = isAct n && isEx n

-- neuron instances
instance Eq N where
  n == m = name n == name m && preds n == preds m
instance Show N where
  show n = name n ++ if null ps then "" else show ps
    where ps = preds n

------------------------
-- Neuron Definitions --
------------------------

count :: [Bool] -> Int
count = length . filter id

fireSI :: Int -> ([Bool] -> Bool) -> ([Bool] -> Bool) -> [Bool] -> Bool
fireSI n s i bs = s ss && i is
  where (ss,is) = splitAt n bs

-- standard neurons
data Std = ExA              -- exogenous action
         | ExL Bool         -- exogenous law: fire?
         | En  Bool [N] [N] -- endogenous: law? [stimulating] [inhibiting]
         deriving (Eq,Show)

instance ND Std where
  stims' (En _ ss _) = ss
  stims' _           = []
  
  inhibs' (En _ _ is) = is
  inhibs' _           = []
  
  isLaw' ExA        = False
  isLaw' (ExL _)    = True
  isLaw' (En l _ _) = l
  
  fire' ExA         = Input
  fire' (ExL b)     = Fire $ const b
  fire' (En _ ss _) = Fire $ fireSI (length ss) or (all not)
  
  attrs' _ = ["shape":="ellipse"]

-- smart constructors
exa n = n :> ExA
exl n b = n :> ExL b
act n ss is = n :> En False ss is
law n ss is = n :> En True  ss is


-- xor neurons
data XOR = XOR Bool [N] deriving (Eq,Show)

instance ND XOR where
  stims'  (XOR _ ns) = ns
  inhibs' _          = []
  isLaw'  (XOR l _ ) = l
  fire'   _          = Fire ((==1) . count)
  attrs'  _          = ["shape":="diamond"]


-- thick neurons
data Thick = Thick Bool [N] [N] deriving (Eq,Show)

instance ND Thick where
  stims'  (Thick _ ss _ ) = ss
  inhibs' (Thick _ _  is) = is
  isLaw'  (Thick l _  _ ) = l
  fire'   (Thick _ ss _ ) = Fire $ fireSI (length ss) ((>1) . count) (all not)
  attrs'  _               = ["penwidth":="3"]


-------------------------
-- Graphs and Diagrams --
-------------------------

-- neuron graph
newtype G = G [N] deriving (Eq,Show)

-- neuron diagram: a graph instantiated with inputs for exogenous actions
data D = D G [Bool] deriving (Eq,Show)

-- nodes in this graph
nodes :: G -> [N]
nodes (G ns) = nub (ns ++ concatMap upstream ns)

-- names of all sink nodes
sinks :: G -> [N]
sinks (G ns) = ns

-- names of all source nodes
sources :: G -> [N]
sources = filter isEx . nodes

graph :: D -> G
graph (D g _) = g

inVals :: D -> [Bool]
inVals (D _ bs) = bs

nodesD :: D -> [N]
nodesD = nodes . graph

sinksD :: D -> [N]
sinksD = sinks . graph

sourcesD :: D -> [N]
sourcesD = sources . graph


-------------------------
-- Dealing with Inputs --
-------------------------

-- input map: maps input neuron names to values
type InMap = [(Name,Bool)]

-- names of all input nodes
inNames :: G -> [Name]
inNames = map name . filter isIn . nodes

-- names of all output nodes
outNames :: G -> [Name]
outNames = map name . sinks

-- number of input nodes
numIns :: G -> Int
numIns = length . inNames

-- generate an input map from a graph and list of inputs
inMap :: G -> [Bool] -> InMap
inMap g i = zip (inNames g) i

-- lookup an input neuron in the input map
look :: Name -> InMap -> Bool
look n m | Just v <- lookup n m = v
         | otherwise = error $ "No value for input neuron: " ++ n

-- generate all inputs of a given length
allIns :: Int -> [[Bool]]
allIns 0 = [[]]
allIns n = map (False:) t ++ map (True:) t
  where t = allIns (n-1)

-- all diagrams for a given graph
allDs :: G -> [D]
allDs g = map (D g) $ (allIns . numIns) g

-- generate n generic neuron names
genNames :: Int -> [Name]
genNames n = map (:[]) $ take n ['A'..]

-- generate n generic input neurons
genNs :: Int -> [N]
genNs = map (:> ExA) . genNames


----------------------
-- Firing Semantics --
----------------------

-- determine the firing state of a neuron in some diagram
evalN :: D -> N -> Bool
evalN (D g i) = evalN' (inMap g i)

-- determine the firing state of a neuron given an input map
evalN' :: InMap -> N -> Bool
evalN' m n = case fire n of
               Input  -> look (name n) m
               Fire f -> f $ map (evalN' m) (preds n)

-- determine the firing state of all sinks of a neuron diagram
eval :: D -> [Bool]
eval d = map (evalN d) (sinksD d)


----------------------
-- Causal Semantics --
----------------------

type Cause  = [Name]  -- conjunction
type Causes = [Cause] -- disjunction of conjunctions

cond :: Name -> Bool -> Name
cond n b = n ++ ':' : if b then "T" else "F"

showC :: Cause -> String
showC = concat . intersperse "^"

showCs :: Causes -> String
showCs = concat . intersperse " v " . map showC

flatten :: [[Causes]] -> Causes
flatten = simplify . concat . (map . map) concat . map cross


--
-- Counterfactual causes
--

-- argument record
type Args = [(Name,Bool)]

-- all alterations of an argument record
alts :: Args -> [Args]
alts a = delete a (map (zip (map fst a)) (allIns (length a)))

altToCause :: Args -> Args -> Cause
altToCause orig alt = [fst a | a <- alt, a `notElem` orig]

-- counterfactual causes of some function for some argument
counters :: ([Bool] -> Bool) -> Args -> Causes
counters f a = (simplify . cross) [altToCause a a' | a' <- alts a, eval a /= eval a']
  where eval a = f (map snd a)

-- generate all generic argument records of length n
genArgs :: Int -> [Args]
genArgs n = map (zip (genNames n)) (allIns n)

-- turn a graph into a boolean function
graphToFun :: G -> [Bool] -> [Bool]
graphToFun g bs | Just d <- find ((bs==) . inVals) (allDs g) = eval d
                | otherwise = error "Invalid graph input."

-- counterfactual causes of each sink in a diagram
countersD :: D -> [Causes]
countersD (D g is) = [counters f as | f <- fs]
  where as = zip (inNames g) is
        fs = [\bs -> graphToFun g bs !! i | i <- [0 .. length (sinks g)-1]]

-- basic counterfactual causes of each sink in a diagram
basicD :: D -> [Causes]
basicD = filter ((==1) . length) . countersD

-- counterfactual causes of each sink, for each combination of inputs
countersG :: G -> [[Causes]]
countersG = map countersD . allDs


--
-- Flow causes
--

type CausesN = (N -> Bool) -> N -> Causes

-- causes of each sink in a diagram
causesD :: CausesN -> D -> [Causes]
causesD f d = map (f (evalN d)) (sinksD d)

-- causes of each sink, for each combination of inputs
causesG :: CausesN -> G -> [[Causes]]
causesG f = map (causesD f) . allDs


-- flow semantics on standard neurons
std :: CausesN
std e n | isAct n      = [[name n]]
        | isEn  n, e n = allOf (cs is) `andOneOf` cs (filter e ss)
        | isEn  n      = allOf (cs ss) `orOneOf`  cs (filter e is)
        | otherwise    = [[]]
  where cs = map (std e)
        ss = stims n
        is = inhibs n


-- flow semantics on generic neurons (implementing the paper)

-- immediate causes of a node
imm :: CausesN
imm e n | isAct n   = [[name n]] -- actions are uncaused
        | otherwise = case fire n of
                        Fire f -> counters f [(name p, e p) | p <- preds n]
                        Input  -> [[]]

-- ultimate causes of a node
ult :: CausesN
ult e n = (flatten . (map . map) expand) (imm e n)
  where 
    expand m | m == name n = [[m]]
             | otherwise   = case find ((m==) . name) (preds n) of
                               Just p  -> ult e p
                               Nothing -> error ("coundn't expand cause: " ++ m)


--
-- Hall's approach (broken)
--

power :: [a] -> [[a]]
power []     = [[]]
power (a:as) = map (a:) p ++ p where p = power as

evalRedN :: D -> [N] -> N -> Bool
evalRedN (D g i) = evalRedN' (inMap g i)

evalRedN' :: InMap -> [N] -> N -> Bool
evalRedN' m s n | n `elem` s = False
                | otherwise  = case fire n of
                    Input  -> look (name n) m
                    Fire f -> f $ map (evalRedN' m s) (preds n)

hallN :: D -> N -> Causes
hallN d n | not (eval [] n) = [["N/A"]]
          | otherwise       = [[name p] | p <- up, eval [] p, any (try p) (power up)]
  where up      = upstream n
        eval    = evalRedN d
        try p s = eval s n && not (eval (p:s) n)

hallD :: D -> [Causes]
hallD d = map (hallN d) (sinksD d)


-------------
-- Testing --
-------------

test :: G -> IO ()
test g = do --print g
            entry "Basic Counterfactual" basicD
            entry "Robust Counterfactual" countersD
            --entry "Standard Flow" (causesD std)
            entry "Ultimate Flow" (causesD ult)
            --entry "Hall's Approach" hallD
  where
    entry s f = putStr s >> putStrLn ":" >> mapM_ (putStrLn . line f) (allDs g)
    line  f d = concat ["  ",is," => ",os," <~ ",cs]
      where 
        nodes ["N/A"] = ["N/A"] -- hack
        nodes ns = [cond (name n) (evalN d n) | n <- nodesD d, name n `elem` ns]
        is = showL $ nodes (inNames g)
        os = showL $ nodes (outNames g)
        cs = showL $ map showCs $ (map . map) nodes (f d)


--------------
-- Examples --
--------------

shoot = G [dead]
  where pull = exa "Pull"
        jam  = exa "Jam"
        dead = law "Dead" [pull] [jam]

-- desert traveler problem (preemption)
desert = G [dead]
  where poison = exa "Poison"
        poke   = exa "Poke"
        drink  = law "Drink"  [poison]       [poke]
        thirst = law "Thirst" [poke]         []
        dead   = law "Dead"   [drink,thirst] []

-- boulder problem (transitivity)
boulder = G [live]
  where boulder = exa "Boulder"
        health  = exl "Health"  True
        duck    = act "Duck"    [boulder] []
        crush   = law "Crush"   [boulder] [duck]
        live    = law "Live"    [health]  [crush]

-- boulder problem (transitivity)
boulderDead = G [dead]
  where boulder = exa "Boulder"
        duck    = act "Duck"    [boulder] []
        dead    = law "Dead"   [boulder] [duck]
        --crush   = law "Crush"   [boulder] [duck]
        --dead    = law "Dead"    [crush]   []

-- assassin-guard problem
assassin = G [live]
  where health   = exl "Health"   True
        assassin = exa "Assassin"
        guard    = act "Guard"    [poison]   []
        poison   = law "Poison"   [assassin] []
        antidote = law "Antidote" [guard]    []
        toxin    = "Toxin" :> XOR True [poison,antidote]
        live     = law "Live"     [health]   [toxin]

-- short circuit problem (Fig. 5 in Hitchcock09)
short = G [e]
  where a = exa "A"
        c = exa "C"
        b = law "B" [c] []
        d = law "D" [c] []
        f = law "F" [d] [b]
        e = law "E" [a] [f]

doc = G [dead]
  where doc  = exa "Doctor"
        sick = exa "Sick"
        cure = law "Cure"   [doc]  []
        dead = law "Dead"   [sick] [cure]
        

docs = G [dead]
  where docA = exa "A"
        docB = exa "B"
        cure = "Cure" :> Thick True [docA,docB] []
        sick = exl "Sick" True
        dead = law "Dead" [sick] [cure] 

ndocs = G [dead]
  where ndocA = exa "!DocA"
        ndocB = exa "!DocB"
        true = exl "True" True
        docA = law "DocA" [true] [ndocA]
        docB = law "DocB" [true] [ndocB]
        cure = "Cure" :> Thick True [docA,docB] []
        sick = exl "Sick" True
        dead = law "Dead" [sick] [cure] 

boyGirl = G [hasBG]
  where true = exl "True" True
        b1 = exa "B1"
        b2 = exa "B2"
        bb = "BB" :> Thick True [b1,b2] []
        bg = law "BG" [b1]   [b2]
        gb = law "GB" [b2]   [b1]
        gg = law "GG" [true] [b1,b2]
        hasB = law "HasB" [bb,bg,gb] []
        hasG = law "HasG" [bg,gb,gg] []
        hasBG = "HasG | HasB" :> Thick True [hasB,hasG] []


desert' = D desert [True,True]
boulder' = D boulder [True]
assassin' = D assassin [True]
short' = D short [True,True]
docs' = D docs [False,False]
ndocs' = D ndocs [True,True]


---------------------------
-- List Helper Functions --
---------------------------

simplify :: Ord a => [[a]] -> [[a]]
simplify [[]] = [[]]
simplify l    = nubBy subset . delete [] . sortBy (compare `on` length) . map (sort . nub) $ l
  where subset y x = intersect x y == x
  
-- [[[1,2],[3]],[[4],[5]]] -> [[1,2,4],[1,2,5],[3,4],[3,5]]
allOf :: Ord a => [[[a]]] -> [[a]]
allOf = simplify . map concat . cross

-- [[1,2],[3]] -> [[[4,5],[6]],[[7]]] -> [[1,2],[3],[4,5],[6],[7]]
orOneOf :: Ord a => [[a]] -> [[[a]]] -> [[a]]
orOneOf cs css = simplify [c | c <- cs ++ concat css, not (null c)]

-- [[1,2],[3]] -> [[[4,5],[6]],[[7]]] -> [[1,2,4,5],[1,2,6],[1,2,7],[3,4,5],[3,6],[3,7]]
andOneOf :: Ord a => [[a]] -> [[[a]]] -> [[a]]
andOneOf cs css = allOf [cs, concat css]

cross :: [[a]] -> [[a]]
cross []       = [[]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]

cross' :: Ord a => [[a]] -> [[a]]
cross' []  = [[]]
cross' xss = (nub . map sort . concat) [ [y:ys | y <- xs, ys <- cross' (xss \\ [xs])] | xs <- xss ]

showL :: [String] -> String
showL l = "[" ++ concat (intersperse "," l) ++ "]"


--------------------
-- Graphviz Stuff --
--------------------

-- There is a Graphviz library on Hackage...
-- Could switch to that, but probably overkill.

type Id = Int
type IdMap = [(Name,Id)]

-- graphviz attribute
data Attr = String := String deriving Eq

instance Show Attr where
  show (n := v) = n ++ "=" ++ v

idMap :: G -> IdMap
idMap = flip zip [1..] . map name . nodes

idOf :: IdMap -> N -> Id
idOf m n | Just i <- lookup (name n) m = i

graphvizN :: IdMap -> Maybe InMap -> N -> String
graphvizN ids ins n = i ++ show (attrs n ++ as) ++ ";"
                        ++ concatMap (edge "normal") (stims n)
                        ++ concatMap (edge "dot")    (inhibs n)
  where i = show (idOf ids n)
             -- can't do (show (name n ++ ... "§" ...)) here, it messes up UTF-8
        as = "label" := ("\"" ++ name n ++ (if isLaw n then {-"§"-}"" else "") ++ "\"")
           : "width" := "1"
           : case ins of
               Just m  -> if evalN' m n then ["style":="filled","fillcolor":="gray"] else []
               Nothing -> ["style":="dashed"]
        edge h n' = show (idOf ids n') ++ "->" ++ i ++ "[arrowhead=" ++ h ++ "];"

graphvizG' :: Maybe InMap -> G -> String
graphvizG' m g = "digraph{rankdir=LR;margin=0.05;" ++ ns ++ "}"
  where ns = concatMap (graphvizN (idMap g) m) (nodes g)

graphvizG :: G -> String
graphvizG = graphvizG' Nothing

graphvizD :: D -> String
graphvizD (D g i) = graphvizG' (Just (inMap g i)) g

-- pop up a graphviz view of a diagram
view :: (a -> String) -> a -> IO ()
view f a = writeFile "view.dot" (f a) >> system "open view.dot" >> return ()

viewG = view graphvizG
viewD = view graphvizD
