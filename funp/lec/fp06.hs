{-# OPTIONS_GHC -XStandaloneDeriving #-}
module Fp06 where


-- Производные представители (Derived Instances)
data Point a = Point a a   deriving Eq

-- Point 3 5 == Point 3 5

-- В GHC можно и отдельно (с ключом -XStandaloneDeriving)
deriving instance Show a => Show (Point a)




-- вспомогательная эмуляция низкоуровневой реализации
eqInt :: Int -> Int -> Bool
eqInt x y = x == y

-- словарь для Eq', то есть запись из его методов
data Eq' a = MkEq (a -> a -> Bool) (a -> a -> Bool)
-- функции-селекторы выбирают методы равенства и неравенства из этого словаря
eq (MkEq e _) = e
ne (MkEq _ n) = n

-- объявления instance транслируются в функции, возвращающие словарь...
dEqInt :: Eq' Int
dEqInt =  MkEq eqInt (\x y -> not $ eqInt x y)
-- ... или в функции, принимающие некоторый словарь и возвращающие более сложный словарь
dEqList :: Eq' a -> Eq' [a]
dEqList (MkEq e _) =  MkEq el (\x y -> not $ el x y)
    where el []     []     = True
          el (x:xs) (y:ys) = x `e` y && xs `el` ys
          el _      _      = False

-- Функция elem теперь принимает словарь в качестве явного параметра
elem'             :: Eq' a -> a -> [a] -> Bool
elem' _ _ []      =  False
elem' d x (y:ys)  =  eq d x y || elem' d x ys
