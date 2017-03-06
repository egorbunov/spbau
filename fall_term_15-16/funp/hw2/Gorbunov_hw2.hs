module Homework2 where
import Test.HUnit
-- Нужно поставить библиотеку hunit:
-- cabal install hunit
-- stack ghci --package hunit

-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n
  | n < 0 = neg_fibs !! abs (fromIntegral n)
  | otherwise = pos_fibs !! fromIntegral n
pos_fibs :: [Integer]
pos_fibs = 0 : 1 : zipWith (+) pos_fibs (tail pos_fibs)
neg_fibs :: [Integer]
neg_fibs = 0 : 1 : zipWith (-) neg_fibs (tail neg_fibs) 

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n
  | abs n < 10 = 1
  | otherwise = 1 + (numberOfDigits (div n 10))

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n
  | abs n < 10 = abs n
  | otherwise = (mod (abs n) 10) + (sumOfDigits (div (abs n) 10))

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' a b = gcd'' (abs a) (abs b) where
  gcd'' a 0 = a
  gcd'' a b = gcd'' b (mod a b)

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = minp' p 0 where
  minp' p n
    | p n = n
    | p (negate n) = negate n
    | otherwise = minp' p (succ n)

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b 
  | abs (b - a) < 1e-2 = (abs (b - a)) * ((f b) + (f a)) / 2
  | otherwise = (integral f a ((a + b) / 2)) + (integral f ((a + b) / 2) b)

-- 6. Определите функцию вычисляющую двойной факториал, то есть произведение натуральных чисел,
--    не превосходящих заданного числа и имеющих ту же четность.
--    (1 балл)
doubleFact :: Integer -> Integer
doubleFact n
  | n <= 0 = 1
  | otherwise = n * (doubleFact (n-2))

-- 7. Реализуйте функцию, находящую элементы следующей рекуррентной последовательности
--    a_0 = 1, a_1 = 2, a_2 = 3, a_{k + 3} = a_{k + 2} + a_{k + 1} - 2a_k
--    (1 балл)
seqA :: Integer -> Integer
seqA n = seqAlist !! (fromIntegral n) where
  seqAlist = 1 : 2 : 3 : zipWith (+) (map (\x->(-2*x)) seqAlist) (zipWith (+) (tail seqAlist) (tail (tail seqAlist)))


-- 8. Реализуйте функцию, возвращающую 1, если число положительное;
--    -1, если отрицательное; 0 в ином случае
--    (1 балл)
sgn :: Integer -> Integer
sgn x 
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0


-- 9. Реализуйте функцию возвращающую модуль переданного ей значения
--    (1 балл)
abs' :: Integer -> Integer
abs' n
  | sgn n < 0 = negate n
  | sgn n > 0 = n
  | otherwise = 0

main :: IO ()
main = fmap (\_ -> ()) $ runTestTT $ test
  $ label "fib"
  [ fib 1001 ~?= 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
  , fib (-10) ~?= (-55)
  , fib (-242) ~?= (-168083057059453008835412295811648513482449585399521)
  , fib 0 ~?= 0
  ] ++ label "numberOfDigits"
  [ numberOfDigits 0 ~?= 1
  , numberOfDigits (-193) ~?= 3
  , numberOfDigits 1009 ~?= 4
  ] ++ label "sumOfDigits"
  [ sumOfDigits 0 ~?= 0
  , sumOfDigits 1 ~?= 1
  , sumOfDigits (-193) ~?= 13
  ] ++ label "gcd'"
  [ gcd' 1 1 ~?= 1
  , gcd' 3 1 ~?= 1
  , gcd' 6 2 ~?= 2
  , gcd' (-405) 105 ~?= 15
  , gcd' (-30) (-70) ~?= 10
  ] ++ label "minp"
  [ minp ((== 0) . (`mod` 1)) ~?= 0
  , minp (\x -> even x && x > 0) ~?= 2
  , minp (\x -> (x < 0) && (x `mod` (-39) == 0) && (x `mod` 11 == 0)) ~?= (-429)
  , minp (< 0) ~?= (-1)
  ] ++ label "doubleFact"
  [ doubleFact 7 ~?= 105
  , doubleFact 8 ~?= 384
  ] ++ label "seqA"
  [ seqA 0 ~?= 1
  , seqA 1 ~?= 2
  , seqA 2 ~?= 3
  , seqA 10 ~?= -6
  , seqA 23 ~?= 279
  , seqA 301 ~?= 1276538859311178639666612897162414
  ] ++ label "sgn"
  [ sgn 0 ~?= 0
  , sgn 42 ~?= 1
  , sgn (-7) ~?= (-1)
  ] ++ label "abs'"
  [ abs' 0 ~?= 0
  , abs' 73 ~?= 73
  , abs' (-198) ~?= 198
  ]

label :: String -> [Test] -> [Test]
label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
