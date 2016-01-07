module EditDistance where

import qualified Data.Map as M
import qualified Control.Monad.State as S


-- Имея на входе два слова, верните их редакционное расстояние
-- Использовать монады если они подходят.

type EditDistance = Int
type DState = M.Map (Int, Int) Int

initState :: Int -> Int -> DState
initState = 

editDistance :: String -> String -> EditDistance
editDistance = undefined
