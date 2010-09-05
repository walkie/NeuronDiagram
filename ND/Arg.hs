
module ND.Arg where

import ND.Neuron
import ND.Diagram

import Data.List (delete)


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

-- create all argument records for a graph
allRecs :: NV a => G a -> [Rec a]
allRecs g = map (rec g) (allInsFor g)

-- all permutations of an argument record
perms :: NV a => Rec a -> [Rec a]
perms r = delete r $ map (zipWith Arg (map argName r)) (allIns (length r))

-- isolate an alteration given the original argument record and a permutation
alt :: Eq a => Rec a -> Rec a -> Alt
alt o = map argName . filter (flip notElem o)

instance NV a => Show (Arg a) where
  show (Arg n a) = n ++ ":" ++ show a--showVal a
