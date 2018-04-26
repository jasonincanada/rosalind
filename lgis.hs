{- Rosalind

   Code:    LGIS
   Problem: Longest Increasing Subsequence
   URL:     http://rosalind.info/problems/lgis/
-}

import Data.Ord          (comparing)
import Data.List         (nub, maximumBy, subsequences, tails)
--import Data.List.Ordered (isSorted, nub)

data TreeF a b = NodeF a [b]

instance Functor (TreeF a) where
  fmap f (NodeF a subs) = NodeF a (fmap f subs)

type Algebra f a = f a -> a

alg :: Ord a => Algebra (TreeF a) Int
alg (NodeF a [])   = 0
alg (NodeF a subs) = 1 + maximum subs

type Pool a = [a]

type Coalgebra f a = a -> f a

coalg :: Ord a => Coalgebra (TreeF a) (a, Pool a)
coalg (span, [])   = NodeF span []
coalg (span, pool) = NodeF span subs
  where subs = [ (head p, filter (>(head p)) p) | p <- tails pool, 
                                                  length p > 0 ] 

-- 1 6 3 5 2 4

hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo f g = g . fmap (hylo f g) . f

isSorted :: Ord a => [a] -> Bool
isSorted []     = True
isSorted (x:[]) = True
isSorted (x:y:rest) = y > x && isSorted (y:rest)

lis :: Ord a => [a] -> [a]
lis = maximumBy (comparing length) . map nub . filter isSorted . subsequences

main = do
  file <- readFile "lgis.input"
  let pool = map read (words $ head $ tail $ lines file) :: [Int]
  --let pool = [5, 1, 4, 2, 3]
  --let longest = hylo coalg alg (0, pool) - 1
  let longest = lis pool
  print $ (length pool, longest)

