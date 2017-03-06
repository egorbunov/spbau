module EditDistance where

import qualified Data.Map as M
import qualified Control.Monad.State as S


-- Имея на входе два слова, верните их редакционное расстояние
-- Использовать монады если они подходят.

type EditDistance = Int
type DState = M.Map (Int, Int) Int

initState :: DState
initState = M.insert (0, 0) 0 M.empty

type SolverAction = S.State DState Int

solverAction :: Int -> Int -> String -> String -> SolverAction
solverAction i j s1 s2 = do
    st <- S.get
    if (i, j) `M.member` st then return $ st M.! (i, j)
                            else helperAction i j s1 s2 st

helperAction :: Int -> Int -> String -> String -> DState -> SolverAction
helperAction i j s1 s2 st
     | i == 0 = do
            S.put $ M.insert (0, j) j st
            return j
     | j == 0 = do
            S.put $ M.insert (i, 0) i st
            return i
     | (s1 !! (i - 1)) == (s2 !! (j - 1)) = do
            ans <- solverAction (i - 1) (j - 1) s1 s2
            S.put $ M.insert (i, j) ans st
            return ans
     | otherwise = do
            u <- solverAction (i - 1) j s1 s2
            l <- solverAction i (j - 1) s1 s2
            d <- solverAction (i - 1) (j - 1) s1 s2
            let ans = succ $ minimum [u, l, d]
            S.put $ M.insert (i, j) ans st
            return ans


editDistance :: String -> String -> EditDistance
editDistance s1 s2 = fst $ S.runState (solverAction (length s1) (length s2) s1 s2) initState