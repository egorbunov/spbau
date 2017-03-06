module MST where

-- Найти по графу его минимальный остов
-- Использовать монады если они подходят.

type Vertex = Int
type Edge = (Vertex, Vertex)
type Score = Int
data ScoredEdge = SE { edge :: Edge
                     , score :: Score
                     }
type Graph = [ScoredEdge]

mst :: Graph -> Graph
mst = undefined
