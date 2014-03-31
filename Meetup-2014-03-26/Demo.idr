module Demo

%default total

data Nat' : Type where
  Z' : Nat'
  S' : Nat' -> Nat'

%name Nat' n,m,o

plus' : Nat' -> Nat' -> Nat'
plus' Z'     m = m
plus' (S' n) m = S' (plus' n m)

data Vect' : Nat -> Type -> Type where
  Nil : Vect' Z a
  (::) : a -> Vect' n a -> Vect' (S n) a

%name Vect' xs,ys,zs

append : Vect' n a -> Vect' m a -> Vect' (n + m) a
append Nil       ys = ys
append (x :: xs) ys = x :: (append xs ys)

zip : Vect' n a -> Vect' n b -> Vect' n (a, b)
zip []        []        = []
zip (x :: xs) (y :: ys) = (x, y) :: (zip xs ys)

take' : (n : Nat) -> Vect (n + m) a -> Vect n a
take' Z     xs = []
take' (S k) (x :: xs) = x :: take' k xs

drop' : (n : Nat) -> Vect (n + m) a -> Vect m a
drop' Z xs = xs
drop' (S k) (x :: xs) = drop' k xs


data SnocList : List a -> Type where
  SnocNil : SnocList (Nil {a=a})
  Snoc    : (xs : List a) -> (x :a) -> SnocList (xs ++ [x])

snocced : (xs : List a) -> SnocList xs
snocced [] = SnocNil
snocced (x :: xs) with (snocced xs)
  snocced (x :: [])          | SnocNil     = Snoc [] x
  snocced (x :: (ys ++ [y])) | (Snoc ys y) = Snoc (x :: ys) y

rot : Nat -> List a -> List a
rot Z       xs                        = xs
rot (S k)   xs       with (snocced xs)
  rot (S k) []          | SnocNil     = []
  rot (S k) (ys ++ [x]) | (Snoc ys x) = rot k (x::ys)

foo : List a -> Nat
foo ([] ++ xs) = 0
foo ((y::ys) ++ xs) = S (foo (ys ++ xs))




