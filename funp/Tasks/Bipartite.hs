module Bipartite where

-- Проверить входной граф на двудольность
-- Использовать монады если они подходят.

type Vertex = Int
type Edge = (Vertex, Vertex)
type Score = Int
type Graph = [Edge]

isBipartite :: Graph -> Bool
isBipartite = undefined
