import Control.Parallel
import Control.DeepSeq

parFold1 :: (a -> a -> a) -> [a] -> a
parFold1 f [] = error "Oh no!"
parFold1 f [x] = x
parFold1 f xs = l `par` r `par` (l `f` r) where
  l = parFold1 f ls
  r = parFold1 f rs
  (ls, rs) = splitAt (length xs `div` 2) xs

main = print $ parFold1 (+) [1..100000 :: Int]

data Pair a b = Pair a b

instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf (Pair a b) = rnf a `seq` rnf b `seq` ()
