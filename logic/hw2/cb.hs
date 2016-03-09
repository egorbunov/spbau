import Data.List

-- Реализуйте алгоритм Кантора-Бернштейна.
cb
    :: (a -> b)         -- Фукнция f, вкладывающая a в b.
    -> (b -> a)         -- Фукнция g, вкладывающая b в a.
    -> (b -> Maybe a)   -- Функция fpi b возвращает Just a, если f a == b. Если такого a не существует, она возвращает Nothing.
    -> (a -> Maybe b)   -- Функция gpi a возвращает Just b, если g b == a. Если такого b не существует, она возвращает Nothing.
    -> (a -> Bool)      -- Функция dfc, такая что dfc a возвращает True тогда и только тогда, когда для любого n существует a0, такой что a == iterate (g . f) a0 !! n
    -> (a -> b, b -> a) -- Результат: биекция между a и b.
cb f g fpi gpi dfc = undefined

-- Пример использования
int_int2 :: Integer -> (Integer,Integer)
int2_int :: (Integer,Integer) -> Integer
(int_int2,int2_int) = cb
    (\a -> (a,0))
    (\(n,m) -> 2^n * 3^m)
    (\(n,m) -> if m == 0 then Just n else Nothing)
    (\a -> let (r2,a' ) = imLog 2 a
               (r3,a'') = imLog 3 a'
           in if a'' == 1 then Just (r2,r3) else Nothing)
    (const False)
  where
    imLog b 0 = (0,0)
    imLog b x = case divMod x b of
                    (d,0) -> let (a,r) = imLog b d in (a + 1, r)
                    _ -> (0,x)

main = do
    print $ map int_int2 [0..100]
    print $ map int2_int [ (n,m) | n <- [0..10], m <- [0..10] ]
    print $ map (int2_int . int_int2) [0..100] -- должен вывести список [0..100]
