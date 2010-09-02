
module ND.GraphViz where

import Data.List (intersperse)
import System (system)

import ND.Neuron
import ND.NeuronTypes
import ND.Diagram
import ND.Fire


------------------------------
-- GraphViz Code Generation --
------------------------------

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


--
-- system stuff: writing files, popping up a graphviz window
--

write' :: FilePath -> (a -> GV) -> a -> IO ()
write' p f a = writeFile p (f a)

view' :: (a -> GV) -> a -> IO ()
view' f a = write' "view.dot" f a >> system "open view.dot" >> return ()

-- write a graphviz file for a graph
writeG :: NV a => FilePath -> G a -> IO ()
writeG = flip write' gvG

-- write a graphviz file for a diagram
write :: NV a => FilePath -> D a -> IO ()
write = flip write' gvD

-- pop up a graphviz view of a graph
viewG :: NV a => G a -> IO ()
viewG = view' gvG

-- pop up a graphviz view of a diagram
view :: NV a => D a -> IO ()
view = view' gvD
