module EditDistance where

import qualified Data.Map as M
import qualified Control.Monad.State as S


-- Имея на входе два слова, верните их редакционное расстояние
-- Использовать монады если они подходят.

type EditDistance = Int

-- (i,j), i for first str, j for second one
type DState = M.Map (Integer, Integer) Integer

initState :: String -> String -> DState
initState s1 s2 = M.insert (0, 0) 0 M.empty

editDistance :: String -> String -> EditDistance
editDistance = undefined
