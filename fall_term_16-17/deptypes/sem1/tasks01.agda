module tasks01 where

const : { A B : Set } → A → B → A
const x _ = x

flip : { A B C : Set } → (A → B → C) → B → A → C
flip f b a = f a b

-- 1. Определить миксфикссный if_then_else_ полиморфный по возвращаемому значению

data Bool : Set where
    true false : Bool

not : Bool → Bool
not true = false
not false = true

infix 7 _||_
_||_ : Bool → Bool → Bool
true || _ = true
false || x = x


if_then_else_ : { A : Set } → Bool → A → A → A
if true then a else _ = a
if false then _ else b = b

-- 2. Определить возведение в степень и факториал для натуральных чисел

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

infix 5 _+_
_+_ : ℕ → ℕ → ℕ
zero + y = y
suc x + y = suc (x + y)

infix 6 _*_
_*_ : ℕ → ℕ → ℕ
zero * y = zero
suc x * y = y + x * y

_^_ : ℕ → ℕ → ℕ
a ^ zero = a
a ^ (suc b) = a * (a ^ b)

_! : ℕ → ℕ
zero ! = suc zero
(suc a) ! = suc a * a !


-- 3. Определите mod и gcd

_-_ : ℕ → ℕ → ℕ
zero - y = zero
suc x - zero = suc x
suc x - suc y = x - y

_<_ : ℕ → ℕ → Bool
zero < zero = false
zero < suc y = true
suc x < zero = false
suc x < suc y = x < y

-- div : ℕ → ℕ → ℕ
-- div x y = if (x < y) then zero else suc (div (x - y) y)

-- div' : ℕ → ℕ → ℕ → ℕ
-- div' zero x y = zero
-- div' (suc c) x y = if (x < y) then zero else suc (div' c (x - y) y)

-- div : ℕ → ℕ → ℕ
-- div x y = div' x x y

mod' : ℕ → ℕ → ℕ → ℕ
mod' zero x y = zero
mod' (suc c) x y = if (x < y) then x else mod' c (x - y) y

mod : ℕ → ℕ → ℕ
mod x y = mod' x x y


gcd : ℕ → ℕ → ℕ
gcd = {!!}

-- 4. Определить (полиморфный) reverse для списков

data List (A : Set) : Set where
  nil : List A
  cons : A → List A → List A

_++_ : {A : Set} → List A → List A → List A
nil ++ ys = ys
cons x xs ++ ys = cons x (xs ++ ys)

-- 5. Реализовать любой алгоритм сортировки

sort : List ℕ → List ℕ
sort = {!!}

-- 6. Докажите ассоциативность ||

data Unit : Set where
  unit : Unit

data Empty : Set where

absurd : {A : Set} → Empty → A
absurd ()

T : Bool → Set
T true = Unit
T false = Empty

infix 3 _==_
_==_ : Bool → Bool → Bool
true == true = true
true == false = false
false == true = false
false == false = true

||-assoc : (x y z : Bool) → T ((x || y) || z == x || (y || z))
||-assoc = {!!}

-- 7. Определите равенство для списков натуральных чисел; докажите, что для любого xs : List ℕ верно, что reverse (reverse xs) равно xs
