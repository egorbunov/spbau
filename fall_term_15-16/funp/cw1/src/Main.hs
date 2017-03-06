module Main where

import System.Environment(getArgs)
import qualified Data.Map as M
import qualified Control.Monad.State as S

-- F(2 * k) =  F(k) * F(k) + F(k - 1) * F(k - 1)
-- F(2 * k + 1) =  F(k) * F(k + 1) + F(k - 1) * F(k)
-- indices start from 1

type FibState = M.Map Integer Integer -- starting value

initState :: FibState
initState = M.insert 0 1 $ M.insert 1 1 M.empty

solver :: Integer -> Integer
solver n = S.evalState (solverAction n) initState

type SolverAction = S.State FibState Integer

solverAction :: Integer -> S.State FibState Integer
solverAction n = do
    s <- S.get 						 -- we tell monad to make evaluation
    if n `M.member` s then return $ s M.! n
                      else helperAction n s

helperAction :: Integer -> FibState -> SolverAction
helperAction n s | even n = do
                        let k = n `div` 2
                        fk <- solverAction k
                        fm1 <- solverAction $ k - 1
                        let newF = fk ^ 2 + fm1 ^ 2
                        S.put $ M.insert n newF s
                        return newF
                 | otherwise = do
                        let k = (n - 1) `div` 2
                        fk <- solverAction k
                        fm1 <- solverAction $ k - 1
                        fp1 <- solverAction $ k + 1
                        let newF = fk * fp1 + fm1 * fk
                        S.put $ M.insert n newF s
                        return newF

main :: IO ()
main = do
    (stringN:_) <- getArgs
    print $ solver (read stringN :: Integer)
