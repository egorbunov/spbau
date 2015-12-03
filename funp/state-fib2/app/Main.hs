
module Main where

import System.Environment(getArgs)
import Control.Monad (forever, unless)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Sequence as DS
import Data.IORef
import qualified Data.Map as M
import qualified Control.Monad.State as S

-- F(2 * k) =  F(k) * F(k) + F(k - 1) * F(k - 1)
-- F(2 * k + 1) =  F(k) * F(k + 1) + F(k - 1) * F(k)
-- indices start from 1

type FibState = M.Map Integer Integer

initState :: FibState
initState = M.insert 0 1 $ M.insert 1 1 M.empty

solver :: Integer -> Integer
solver n = S.evalState (solverAction n) initState

type SolverAction = S.State FibState Integer

solverAction :: Integer -> S.State FibState Integer
solverAction n = do
    s <- S.get
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

type Queue = DS.Seq Integer
type Tasks = TVar Queue
type StateRef = IORef FibState

workerThread :: Tasks -> IO ()
workerThread tasks = do
    ref <- newIORef initState
    forever $ workerHelper tasks ref

atomicWait :: Tasks -> STM Integer
atomicWait tasks = do
    ts <- readTVar tasks
    if null ts then retry
               else do
                let (rest DS.:> x) = DS.viewr ts
                writeTVar tasks rest
                return x

workerHelper :: Tasks -> StateRef -> IO ()
workerHelper tasks ref = do
    t <- atomically $ atomicWait tasks
    s <- readIORef ref
    let (r, s') = S.runState (solverAction t) s
    writeIORef ref s'
    print r

listenerThread :: Tasks -> IO ()
listenerThread tasks = do
    line <- getLine
    unless (null line) $ atomically $ modifyTVar tasks $
        \ts -> (read line :: Integer) DS.<| ts

main :: IO ()
main = do
    taskQueue <- newTVarIO DS.empty
    forkIO $ workerThread taskQueue
    forever $ listenerThread taskQueue
