module ConnectedComponents where

-- По данному графу вернуть список его компонент связности
-- Использовать монады если они подходят.

type Vertex = Int
type Edge = (Vertex, Vertex)
type Score = Int
type Graph = [Edge]
type Component = [Vertex]

components :: Graph -> [Component]
components = undefined
