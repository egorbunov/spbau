module Seminar5 where

mapr f = foldr (\x acc -> f x : acc) [] -- map using foldr

maplnn f = foldl (\acc x -> acc ++ [f x]) [] -- map using foldl, O(n^2)
mapln f = foldl (flip (:)) [] . foldl (\acc x -> f x : acc) [] -- map using foldl, O(n)

unfoldr :: (a -> Maybe (a, b)) -> a -> [b]
unfoldr f z = case f z of
				Nothing -> []
				Just (a, b) -> b : unfoldr f a
-- usage: unfoldr (\x -> if x == 0 then Nothing else Just (x-1, x)) 10


-- Monoids

data List a = Nil | Cons a (List a)

myConcat Nil b = b
myConcat (Cons a as) b = Cons a $ as `myConcat` b

-- :i Monoid ...
instance Monoid (List a) where
	mempty = Nil
	mappend = myConcat

toMonoid a = Cons a Nil

-- type constructor is better to export for external usage, because
-- data constructor allows pattern matching, but it can be harmful, because 
-- it shows all that private shit...

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

newtype SortedList a = SL (List a) deriving (Eq, Show)

toSortedList :: Ord a => [a] -> SortedList a
toSortedList = SL . toMyList . sort

instance Ord a => Monoid (SortedList a) where
	mempty = SL $ Nil
	(SL ax) `mappend` (SL ys) = SL . merge ax ys 

merge :: Ord a => List a -> List a -> List a
merge Nil as = as
merge as Nil = as
merge xs'@(Cons x xs) ys'@(Cons y ys) 
	| x < y = Cons x $ merge xs ys'
	| otherwise = Cons y $ merge xs' ys

-- Sorting

--qsort :: Ord a => [a] -> [a]
--qsort [] = []
--qsort xs'@(x : xs) = (qsort $ filter (<x) xs) ++
--					 (qsort (filter (==x) xs') ++
--					 (qsort )

data Tree a = Leaf | Branch a [Tree a] deriving (Eq, Show)

--instance Foldable (Tree a) where
	
newtype M1 a = M1 (Maybe a)
newtype M2 a = M2 (Maybe a)

instance Monoid (M1 a) where
	mempty = M1 $ Nothing
	a `mappend` b =  