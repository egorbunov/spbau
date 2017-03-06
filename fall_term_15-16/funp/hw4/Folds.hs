module Folds where

import Prelude hiding (foldr, foldl)
import Control.Monad (void)
import Data.List (unfoldr)
import Data.Foldable(Foldable, foldr, foldl)
import Test.HUnit

-- 1. Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список
-- символов, попадающих в заданный парой диапазон. (1 балл)
revRange :: (Char, Char) -> [Char]
revRange (x, y) = unfoldr go (y, x)
    where go = \(a,b) -> if a < b then Nothing else Just (a, (pred a, b))

-- 2. Напишите реализации функций из стандартной библиотеки tails, inits :: [a] -> [[a]] через
--    foldr. (1 балл)
tails' :: [a] -> [[a]]
tails' = foldr f z
    where
        f x tls = ([x] ++ (head tls)) : tls 
        z = [[]]

inits' :: [a] -> [[a]]
inits' = foldr f z
    where
        f x ls = [] : (map (x:) ls)
        z = [[]]

-- 3. Напишите реализацию оператора "безопасного" поиска элемента списка по индексу через foldr.
-- (2 балла)
(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr f z xs n
    where
        f x b = \k -> if k == 0 then Just x else b (k - 1)
        z = \k -> Nothing

-- 4. Напишите реализацию foldl через foldr. (2 балла)
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f z xs = foldr (fun f) ini xs z
    where
        fun f b g = \x -> g (f x b)
        ini = id

-- 5. Напишите для дерева два инстанса Foldable для деревьев: in-order и level-order (3 балла)
data Tree a = Leaf | Branch a (Tree a) (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
    foldr f z Leaf = z
    foldr f z (Branch k l r) = foldr f (f k (foldr f z r)) l

newtype LevelOrder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable LevelOrder where
    foldr f z t = Data.Foldable.foldr f z (bfs [t])
      where bfs [] = []
            bfs ((LevelO Leaf) : xs) = bfs xs
            bfs (LevelO (Branch k l r) : xs) = k : bfs (xs ++ [LevelO l, LevelO r])

------------------------------------------------------------------------------
-- main

tstTree = Branch 6
            (Branch 2
                (Branch 1 Leaf Leaf)
                (Branch 4
                    (Branch 3 Leaf Leaf)
                    (Branch 5 Leaf Leaf)))
            (Branch 7
                Leaf
                (Branch 9 (Branch 8 Leaf Leaf) Leaf))

tB = TestCase . assert

main = void $ runTestTT $ test
      $  label "1. revRange" [ revRange ('F','Q') ~?= "QPONMLKJIHGF"
                             , revRange ('F','F') ~?= "F"
                             , revRange ('Q','F') ~?= ""
                             ]
      ++ label "2. tails, inits" [ tails' [1,2,3] ~?= [[1,2,3],[2,3],[3],[]]
                                 , tails' "" ~?= [""]
                                 , inits' [1,2,3] ~?= [[],[1],[1,2],[1,2,3]]
                                 , inits' "" ~?= [""]
                                 , inits' "abc" ~?= ["","a","ab","abc"]
                                 , tails' "abc" ~?= ["abc","bc","c",""]
                                 ]
       ++ label "3. Safe !!" [ [1..10] !!! 6 ~?= Just 7
                             , [1..10] !!! 10 ~?= Nothing
                             , [1..10] !!! 0 ~?= Just 1
                             , [1..10] !!! (-33) ~?= Nothing
                             , [1..]   !!! 999 ~?= Just 1000
                             ]
       ++ label "4. foldl -> foldr" [ foldl'' (-) 0 [1..10] ~?= foldl (-) 0 [1..10]
                                    , foldl'' (flip (:)) [] [1..10] ~?= foldl (flip (:)) [] [1..10]
                                    , tB $ null $ foldl'' (flip (:)) [] []
                                    ]
       ++ label "5. Foldable Tree" [ tB $ null $ foldl (flip (:)) [] Leaf
                                   , tB $ null $ foldr (:) [] Leaf
                                   , tB $ null $ foldl (flip (:)) [] (LevelO Leaf)
                                   , foldr (:) [] tstTree ~?= [1,2,3,4,5,6,7,8,9]
                                   , foldl (flip (:)) [] tstTree ~?= [9,8,7,6,5,4,3,2,1]
                                   , foldr (:) [] (LevelO tstTree) ~?= [6,2,7,1,4,9,3,5,8]
                                   , foldl (flip (:)) [] (LevelO tstTree) ~?= [8,5,3,9,4,1,7,2,6]
                                   ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i, t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
