module Read where
import Test.HUnit
import Control.Monad (void)

data List a = Nil  | Cons   a (List a) deriving (Eq, Show)
data Tree a = Leaf | Branch a (Tree a) (Tree a)  deriving (Eq, Show)

-- 1. Напишите инстанс Read для списков. (1 балл)

instance (Read a) => Read (List a) where
    readsPrec = undefined

-- 2. Напишите инстанс Read для бинарных деревьев. (2 балла)

instance (Read a) => Read (Tree a) where
    readsPrec = undefined

-- 3. Рассмотрим тип Expr для нетипизированного лямбда-исчисления,
-- напишите для него инстансы Read и Show. (2.5 балла)
-- Для представителя Read может оказаться удобным воспользоваться функцией lex.

type Symb = String

infixl 2 :@

data Expr = Var Symb
            | Expr :@ Expr
            | Lam Symb Expr
                deriving Eq

instance Show Expr where
    show = undefined

instance Read Expr where
    readsPrec = undefined

------------------------------------------------------------------------------
-- main

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

main = void $ runTestTT $ test
      $  label "1. Read List" [ toList (read "[1, 2, 3, 4]" :: List Int) ~?= ([1, 2, 3, 4] :: [Int])
                              , toList (read "[1,2,3,4]" :: List Int) ~?= ([1, 2, 3, 4] :: [Int])
                              , toList (read "[]" :: List Int) ~?= ([] :: [Int])
                              , toList (read "['f', 'o', 'o']" :: List Char) ~?= "foo"
                              ]
      ++ label "2. Read Tree" [ (read "<>" :: Tree Int) ~?= (Leaf :: Tree Int)
                              , (read "<<> 1 <>>" :: Tree Int) ~?= (Branch 1 Leaf Leaf :: Tree Int)
                              , (read "< <<> 7 <<> 9 <>>> 2.0 <<> 42 <>> >" :: Tree Double) ~?= ((Branch 2.0
                                                                                                    (Branch 7
                                                                                                        Leaf
                                                                                                        (Branch 9 Leaf Leaf ))
                                                                                                    (Branch 42 Leaf Leaf)) :: Tree Double)]
      ++ label "3. Read Expr" [ read "x" ~?= Var "x"
                              , read "x y" ~?= (Var "x" :@ Var "y")
                              , read "x y z" ~?= (Var "x" :@ Var "y" :@ Var "z")
                              , read "(x y) z" ~?= (Var "x" :@ Var "y" :@ Var "z")
                              , TestCase $ assert $ read "x (y z)" /= (Var "x" :@ Var "y" :@ Var "z")
                              , read "x (y z)" ~?= (Var "x" :@ (Var "y" :@ Var "z"))
                              , read "\\x -> x y" ~?= Lam "x" (Var "x" :@ Var "y")
                              , read "\\x y -> x y" ~?= Lam "x" (Lam "y" (Var "x" :@ Var "y"))
                              , read (show $ Var "a") ~?= Var "a"
                              , read (show (Var "a" :@ Var "b")) ~?= (Var "a" :@ Var "b")
                              , read (show $ Var "x" :@ Var "y" :@ Var "z") ~?= (Var "x" :@ Var "y" :@ Var "z")
                              , read (show $ Var "x" :@ (Var "y" :@ Var "z")) ~?= (Var "x" :@ (Var "y" :@ Var "z"))
                              , read (show $ Lam "x" (Var "x" :@ Var "y")) ~?= Lam "x" (Var "x" :@ Var "y")
                              , read (show $ Lam "x" (Lam "y" (Var "x" :@ Var "y"))) ~?= Lam "x" (Lam "y" (Var "x" :@ Var "y"))
                              ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i, t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
