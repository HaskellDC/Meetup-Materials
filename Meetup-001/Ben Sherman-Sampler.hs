{-# LANGUAGE Rank2Types #-}

module Sampler where
import Control.Applicative
import System.Random

data Sampler a = Sampler { sample :: RandomGen g => g -> (a, g) }

instance Monad Sampler where
  (Sampler f) >>= g = Sampler $ \r -> let (x, r2) = f r; Sampler h = g x in h r2
  return x = Sampler $ \r -> (x, r)

instance Functor Sampler where
  fmap f (Sampler g) = Sampler $ \r -> let (x, r2) = g r in (f x, r2)

instance Applicative Sampler where
  Sampler f <*> Sampler x = Sampler $ \r -> let (f', r2) = f r; (x', r3) = x r2 in (f' x', r3)
  pure = return

--Get out sample from our sampler
runSample :: Sampler a -> IO a
runSample (Sampler f) = do
  g <- newStdGen
  return (fst (f g))

--Get an infinite list of samples from our sampler
runSamples :: Sampler a -> IO [a]
runSamples s = runSample (sequence (repeat s))

--Sample uniformly from the unit interval
unit :: Sampler Double
unit = Sampler $ randomR (0,1)

--The sum of two numbers independently drawn from a uniform distribution on the unit interval
summed :: Sampler Double
summed = do
  x <- unit
  y <- unit
  return (x + y)

--A similar thing, but semantically not quite identical to the above;
--here, we don't have the ability to feed our algorithm a specific seed.
summed' :: IO Double
summed' = do
  x <- runSample unit
  y <- runSample unit
  return (x + y)

--Sample uniformly from the unit circle
circle :: Sampler (Double, Double)
circle = do
  i <- unit
  return (cos (2*pi*i), sin (2*pi*i))

--A single Bernoulli trial with probability p of success
bernoulli :: Double -> Sampler Bool
bernoulli p = fmap (<p) unit

--The number of successes from n independent Bernoulli trials with probability p of success
binomial :: Int -> Double -> Sampler Int
binomial n p = fmap (length . filter id) $ sequence (replicate n (bernoulli p))

--A higher-order combinator for sampling. This gives us rejection sampling:
--We only accept a sample if it satisfies our predicate.
rejectionSample :: (a -> Bool) -> Sampler a -> Sampler a
rejectionSample p (Sampler f) = Sampler g where
  g r = let (x, r2) = f r in if p x then (x, r2) else g r2

--Sample uniformly (by area) from the unit disk
disk :: Sampler (Double, Double)
disk = rejectionSample (\(x,y) -> x^2 + y^2 <= 1) square where
  square = do
    x <- unit
    y <- unit
    return (2*x - 1, 2*y - 1)
