module Euler where

-- По данному графу найдите цикл Эйлера в нем или укажите, что его нет.
-- Использовать монады если они подходят.

type Vertex = Int
type Edge = (Vertex, Vertex)
type Score = Int
type Graph = [Edge]
type Circuit = [Vertex]

eulerianCurcuit :: Graph -> Either String Circuit
eulerianCurcuit = undefined
