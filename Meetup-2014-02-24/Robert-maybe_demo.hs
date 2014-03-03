-- Adding with safe division in the Maybe monad.

{-
-- From GHC.Base:

class Monad m where
    (>>=)       :: m a -> (a -> m b) -> m b
    (>>)        :: m a -> m b -> m b
    return      :: a -> m a
    fail        :: String -> m a

    m >> k      = m >>= \_ -> k
    fail s      = error s

-- From Control.Maybe:

data  Maybe a  =  Nothing | Just a
  deriving (Eq, Ord)

instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return              = Just
    fail _              = Nothing

-- From Control.Applicative:

instance Applicative Maybe where
    pure = return
    (<*>) = ap

-- From Control.Monad:

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 id

liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
-}

{-
 - Monads solve the following problem.  How can we compose effectful
 - functions?
 -}

-- ********************************

-- Composible non-monadic functions:

add0  y = \x -> x + y
sub0  y = \x -> x - y
mul0  y = \x -> x * y
sdiv0 y = \x -> if y == 0 then 0 else x / y

f a b c d e = f1 a
  where
    f1   =  mul0 e . sub0 d . sdiv0 c . add0 b
 -- f1 x = (mul0 e . sub0 d . sdiv0 c . add0 b) x

-- sdiv0 (safe division) doesn't handle errors very well.

-- ********************************

-- Composible monadic functions:

-- These functions are written so that the results look like the functions
-- expected by bind:  \a -> Maybe b.

-- Haskell deduces these types for the functions:
--
-- add, sub, mul :: Num a                => a -> a -> Maybe a
-- sdiv          :: (Eq a, Fractional a) => a -> a -> Maybe a

add  y = \x -> Just (x + y)
sub  y = \x -> Just (x - y)
mul  y = \x -> Just (x * y)
sdiv y = \x -> if y == 0 then Nothing else Just (x / y)

-- These functions are equivalent to the first set:

add2  = \y x -> Just (x + y)
sub2  = \y x -> Just (x - y)
mul2  = \y x -> Just (x * y)
sdiv2 = \y x -> if y == 0 then Nothing else Just (x / y)

-- These functions are also equivalent to the first set:

add3  y x = Just (x + y)
sub3  y x = Just (x - y)
mul3  y x = Just (x * y)
sdiv3 y x = if y == 0 then Nothing else Just (x / y)

-- Tests:

test01 = f 3 4 2 2 3  -- non-monadic
test02 = f 3 4 0 2 3  -- non-monadic

test03 = return 3 >>= add 4 >>= sdiv 2 >>= sub 2 >>= mul 3
test04 = return 3 >>= add 4 >>= sdiv 0 >>= sub 2 >>= mul 3

test05 = add 4 3 >>= sdiv 2 >>= sub 2 >>= mul 3
test06 = add 4 3 >>= sdiv 0 >>= sub 2 >>= mul 3

test07 = return 3 >>= add2 4 >>= sdiv2 2 >>= sub2 2 >>= mul2 3
test08 = return 3 >>= add2 4 >>= sdiv2 0 >>= sub2 2 >>= mul2 3

test09 = return 3 >>= add3 4 >>= sdiv3 2 >>= sub3 2 >>= mul3 3
test10 = return 3 >>= add3 4 >>= sdiv3 0 >>= sub3 2 >>= mul3 3

testx = return 3 >>= \a -> add 4 a
                 >>= \b -> sdiv 2 b
                 >>= \c -> sub 2 c
                 >>= \d -> mul 3 d

test11 = do a <- return 3
            b <- add 4 a
            c <- sdiv 2 b
            d <- sub 2 c
            e <- mul 3 d
            return e

test12 = do a <- return 3
            b <- add 4 a
            c <- sdiv 2 b
            d <- sub 2 c
            mul 3 d

test13 = do a <- return 3
            b <- add 4 a
            c <- sdiv 0 b
            d <- sub 2 c
            e <- mul 3 d
            return e

test14 = do a <- return 3
            b <- add 4 a
            c <- sdiv 0 b
            d <- sub 2 c
            mul 3 d

g a = do b <- add 4 a
         c <- sdiv 2 b
         d <- sub 2 c
         mul 3 d

test15 = g 3

h a b c d e = return a >>= add b >>= sdiv c >>= sub d >>= mul e

test16 = h 3 4 2 2 3

test17 = h 3 4 0 2 3

-- Without the Maybe Monad:

test18 = case add 4 3 of
           Nothing -> Nothing
           Just r1 -> case sdiv 2 r1 of
             Nothing -> Nothing
             Just r2 -> case sub 2 r2 of
               Nothing -> Nothing
               Just r3 -> case mul 3 r3 of
                 Nothing -> Nothing
                 Just r4 -> Just r4

test19 = case add 4 3 of
           Nothing -> Nothing
           Just r1 -> case sdiv 0 r1 of
             Nothing -> Nothing
             Just r2 -> case sub 2 r2 of
               Nothing -> Nothing
               Just r3 -> case mul 3 r3 of
                 Nothing -> Nothing
                 Just r4 -> Just r4

putTest name test = putStrLn $ name ++ ": " ++ show test

main = do putTest "test01" test01
          putTest "test02" test02
          putTest "test03" test03
          putTest "test04" test04
          putTest "test05" test05
          putTest "test06" test06
          putTest "test07" test07
          putTest "test08" test08
          putTest "test09" test09
          putTest "test10" test10
          putTest "test11" test11
          putTest "test12" test12
          putTest "test13" test13
          putTest "test14" test14
          putTest "test15" test15
          putTest "test16" test16
          putTest "test17" test17
          putTest "test18" test18
          putTest "test19" test19

