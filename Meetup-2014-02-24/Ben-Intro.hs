{-# LANGUAGE GADTs, RankNTypes #-}
import Prelude (Show)

--

data Bool where
  True :: Bool
  False :: Bool
  deriving Show

haskellIsCool :: Bool
haskellIsCool = True

--

not :: Bool -> Bool
not True = False
not False = True

false :: Bool
false = not True

ident :: Bool -> Bool
ident x = x

alwaysTrue :: Bool -> Bool
alwaysTrue x = True

--

or :: Bool -> (Bool -> Bool)
or True = alwaysTrue
or False = ident

false' :: Bool
false' = (or False) False

--

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
  deriving Show

two :: Nat
two = Succ (Succ Zero)

even :: Nat -> Bool
even Zero = True
even (Succ Zero) = False
even (Succ (Succ n)) = even n

--

id :: forall a. a -> a
id x = x

const :: a -> b -> a
const x y = x

compose :: (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f x)

--

data Twoople a b where
  Two :: forall a. forall b. a -> b -> Twoople a b

swap :: Twoople a b -> Twoople b a
swap (Two x y) = Two y x

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a
  deriving Show

map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

--

answer :: List Bool
answer = map not (Cons True (Cons False Nil))

--

class Eq a where
  eq :: a -> a -> Bool

instance Eq Nat where
  eq Zero Zero = True
  eq (Succ m) Zero = False
  eq Zero (Succ n) = False
  eq (Succ m) (Succ n) = eq m n

elem :: forall a. Eq a => a -> List a -> Bool
elem x Nil = False
elem x (Cons y ys) = or (eq x y) (elem x ys)

--

nats :: List Nat
nats = startFrom Zero where startFrom n = Cons n (startFrom (Succ n))
  
data Maybe a where
  Nothing :: Maybe a
  Just :: a -> Maybe a
  deriving Show

second :: List a -> Maybe a
second (Cons x (Cons y zs)) = Just y
second (Cons x Nil)         = Nothing
second Nil                  = Nothing

answer2 = second (nats)
