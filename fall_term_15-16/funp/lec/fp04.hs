module Fp04 where
x = 2               -- глобальное
y = 42              -- глобальное
foo = let z = x + y -- глобальное (foo) локальное (z)
      in print z    -- отступ (layout rule)


add x y = x + y         -- определение add
add' x = \y -> x + y
add''  = \x y -> x + y

fortyTwo = add 40 2     -- вызов add

--oops = print add 1 2
good = print (add 1 2)

z = 1         -- ok, связали
--z = 2         -- ошибка
q q = \q -> q -- ok, но...

factorial n = if n > 1
              then n * factorial (n-1)
              else 1

factorial' n = helper 1 n
helper acc n = if n > 1
               then helper (acc * n) (n - 1)
               else acc

factorial'' n' = helper 1 n'
   where helper acc n = if n > 1
                        then helper (acc * n) (n - 1)
                        else acc
factorial''' n' = 
   let helper acc n = if n > 1
                      then helper (acc * n) (n - 1)
                      else acc
   in helper 1 n'


mult :: Integer -> (Integer -> Integer)
mult x1 x2 = x1 * x2

-- Операторы

a *+* b = a * a + b * b

res = 3 *+* 4 


(**+**) a b = a * a * a + b * b * b

res1 = (**+**) 2 3
res2 = 2 **+** 3


x `plus` y = x + y 

res3 = 2 `plus` 3
res4 = plus 2 3


infixl 6  *+*, **+**, `plus`


