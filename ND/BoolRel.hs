
module ND.BoolRel where

import Data.Function (on)
import Data.Monoid
import Data.List


-----------------------
-- Boolean Relations --
-----------------------

newtype Con a = Con [a] -- conjunction
newtype Dis a = Dis [a] -- disjunction

type DC a = Dis (Con a) -- disjunction of conjunctions

-- logical relation (better name?)
class (Functor r, Monad r) => Rel r where
  toList   :: r a -> [a]
  fromList :: [a] -> r a

toList2 :: (Rel r, Rel s) => r (s a) -> [[a]]
toList2 = toList . fmap toList

fromList2 :: (Rel r, Rel s) => [[a]] -> r (s a)
fromList2 = fromList . map fromList

onList :: Rel r => ([a] -> [b]) -> r a -> r b
onList f = fromList . f . toList

onList2 :: (Rel r, Rel s) => ([[a]] -> [[b]]) -> r (s a) -> r (s b)
onList2 f = fromList2 . f . toList2

-- (set) equality of two expressions
relEq :: (Rel r, Ord a) => r a -> r a -> Bool
relEq = (==) `on` (toList . simplify)

-- exploit associativity to remove a relation nesting
assoc :: Rel r => r (r a) -> r a
assoc = onList (concatMap toList)

-- exploit distiributivity to swap the nesting of relations
dist :: (Rel r, Rel s) => r (s a) -> s (r a)
dist = fromList2 . dist' . toList2
  where dist' []       = []
        dist' [xs]     = [[y]  | y <- xs]
        dist' (xs:xss) = [y:ys | y <- xs, ys <- dist' xss]

-- flatten a nested DC expression
-- (the below more general type looks complicated and is probably not needed)
--flatten :: (Rel r, Rel s) => r (s (r (s a))) -> r (s a)
flatten :: DC (DC a) -> DC a
flatten = fmap assoc . assoc . fmap dist

-- remove redundant terms from an expression
simplify :: (Rel r, Ord a) => r a -> r a
simplify = onList (sort . nub)

-- remove redundant terms from nested expressions
simplify2 :: (Rel r, Rel s, Ord a) => r (s a) -> r (s a)
simplify2 = onList2 (nubBy subset . sort) . fmap simplify
  where sort = sortBy (compare `on` length)
        subset x y = x == z || y == z where z = intersect x y


---------------
-- Instances --
---------------

instance Rel Con where
  toList (Con as) = as
  fromList = Con
instance Rel Dis where
  toList (Dis as) = as
  fromList = Dis

instance Ord a => Eq (Con a) where (==) = relEq
instance Ord a => Eq (Dis a) where (==) = relEq

instance Functor Con where
  fmap f (Con as) = Con (map f as)
instance Functor Dis where
  fmap f (Dis as) = Dis (map f as)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

instance Monad Con where
  return a     = Con [a]
  Con as >>= f = Con (concat [bs | Con bs <- map f as])
  fail _       = Con []

instance Monad Dis where
  return a     = Dis [a]
  Dis as >>= f = Dis (concat [bs | Dis bs <- map f as])
  fail _       = Dis []

parens :: String -> String
parens s = "(" ++ s ++ ")"

sepBy :: (Show a, Rel r) => String -> r a -> String
sepBy s = concat . intersperse s . map show . toList

-- These Show instances look nice for DC expressions, but could be confusing
-- in other contexts.  Uncomment the parens functions for a more explicit
-- representation.
instance Show a => Show (Con a) where
  show = {- parens . -} sepBy "^"
instance Show a => Show (Dis a) where
  show = {- parens . -} sepBy " v "
