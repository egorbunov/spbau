import Data.List(intercalate)

type Sort = String
type Function = String

data Signature = Signature
    { sorts :: [Sort]
    , functions :: [(Function,([Sort],Sort))]
    }

singleSorted :: Sort -> [(Function,Int)] -> Signature
singleSorted sort funs = Signature
    { sorts = [sort]
    , functions = map (\(f,n) -> (f, (replicate n sort, sort))) funs
    }

type Variable = String
data Term = Var Variable Sort | Fun Function [Term]

instance Show Term where
    show (Var v _) = v
    show (Fun f ts) = f ++ "(" ++ intercalate ", " (map show ts) ++ ")"

-- Реализуйте функцию, проверяющую что предтерм является корректным термом в данной сигнатуре и возвращающую его сорт, если он корректен.
isCorrectTerm :: Signature -> Term -> Maybe Sort
isCorrectTerm = undefined

infix 4 :==
data Formula = Term :== Term

instance Show Formula where
    show (t :== t') = show t ++ " = " ++ show t'

data Theory = Theory
    { signature :: Signature
    , axioms :: [Formula]
    }

data Proof = Axiom Formula | Refl Term | Sym Proof | Trans Proof Proof | Cong Function [Proof] | App Proof Variable Term

-- Реализуйте функцию, проверяющую, что доказательство является корректным и возвращающую теорему, которую оно доказывает, если оно корректно.
isCorrectProof :: Theory -> Proof -> Maybe Formula
isCorrectProof = undefined

-- Запишите любую теорию и доказательство любой теоремы из домашнего задания.
someTheory :: Theory
someTheory = undefined

someProof :: Proof
someProof = undefined

-- Пример

var x = Var x "D"
x *. y = Fun "*" [x,y]
inv x = Fun "inv" [x]
e = Fun "1" []

assoc = (var "x" *. var "y") *. var "z" :== var "x" *. (var "y" *. var "z")
e_left = e *. var "x" :== var "x"
e_right = var "x" *. e :== var "x"
inv_left = inv (var "x") *. var "x" :== e

groupTheory :: Theory
groupTheory = Theory
    { signature = singleSorted "D" [("*",2), ("1",0), ("inv",1)]
    , axioms = [assoc, e_right, e_left, inv_left]
    }

-- if p_i proves that t_i = t_{i+1}, then trans [p_1, ... p_n] proves that t_1 = t_{n+1}.
trans :: [Proof] -> Proof
trans [p] = p
trans (p:ps) = Trans p (trans ps)

-- if p proves that y * y = y, then (idempotent_is_identity y p) proves that y = 1.
idempotent_is_identity :: Term -> Proof -> Proof
idempotent_is_identity y p = trans
    [ Sym $ App (Axiom e_left) "x" y                            -- y = 1 * y
    , Cong "*" [Sym $ App (Axiom inv_left) "x" y, Refl y]       -- 1 * y = (inv(y) * y) * y
    , App (App (App (Axiom assoc) "x" (inv y)) "y" y) "z" y     -- (inv(y) * y) * y = inv(y) * (y * y)
    , Cong "*" [Refl $ inv y, p]                                -- inv(y) * (y * y) = inv(y) * y
    , App (Axiom inv_left) "x" y                                -- inv(y) * y = 1
    ]

-- inv_right proves that (x * inv x = 1).
inv_right :: Proof
inv_right = idempotent_is_identity (x *. inv x) $ trans
    [ App (App (Axiom assoc) "y" (inv x)) "z" (x *. inv x)                                                  -- (x * inv(x)) * (x * inv(x)) = x * (inv(x) * (x * inv(x)))
    , Cong "*" [Refl x, Sym $ App (App (App (Axiom assoc) "x" (inv x)) "y" x) "z" (inv x)]                  -- x * (inv(x) * (x * inv(x))) = x * ((inv(x) * x) * inv(x))
    , Cong "*" [Refl x, Trans (Cong "*" [Axiom inv_left, Refl $ inv x]) $ App (Axiom e_left) "x" (inv x)]   -- x * ((inv(x) * x) * inv(x)) = x * inv(x)
    ]
  where
    x = var "x"

main = do
    print (isCorrectProof groupTheory inv_right)
    print (isCorrectProof someTheory someProof)
