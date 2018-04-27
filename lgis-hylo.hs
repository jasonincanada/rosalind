{- Rosalind

   Code:    LGIS
   Problem: Longest Increasing Subsequence
   URL:     http://rosalind.info/problems/lgis/

   Remarks: This implementation uses a hylomorphism to build a tree of
            all valid sequences, then reduces it to a longest sequence.
            It considers every possible valid sequence, so it's far too
            inefficient to run on more than around a hundred numbers.
            The problem set I was given has 8,739 numbers.

            The algorithm first groups consecutive values into a single
            logical Span.  For example, [1, 2, 3, 20, 21] is mapped to
            [Span 1 3, Span 20 2].  I hoped this would give the algo a
            reduced combinatoric space, but the input I was given had
            only a single pair of consecutive numbers, so this approach
            was not effective in reducing run time.  Nonetheless it was
            a good exercise in thinking about type parameters: Span is
            almost ready to be used for other orderable types, but it
            needs further work to account for what "+1" means in other
            types.
-}

{-# LANGUAGE DeriveFunctor #-}

import Data.Ord  (comparing)
import Data.List (tails, maximumBy)

-- Span 4 3 represents [4, 5, 6]
data Span a = Span a Int
              deriving (Show, Eq)

-- Compare two spans only by their starting number, disregarding length
instance Ord a => Ord (Span a) where
  (Span n _) `compare` (Span m _) = n `compare` m

data TreeF a b = NodeF (Span a) [b]
                 deriving (Functor)

type Algebra f a = f a -> a

algebra :: Ord a => Algebra (TreeF a) [Span a]
algebra (NodeF span [])   = [span]
algebra (NodeF span subs) = span : maximumBy (comparing slength) subs
  where slength :: [Span a] -> Int
        slength spans = sum $ map slen spans
          where slen (Span _ n) = n

type Pool a = [Span a]
type Coalgebra f a = a -> f a

coalgebra :: Ord a => Coalgebra (TreeF a) (Span a, Pool a)
coalgebra (span, [])   = NodeF span []
coalgebra (span, pool) = NodeF span subs
  where subs = [ let start = head p
                 in  (start, filter (>start) p) | p <- tails pool,
                                                  not . null $ p ]

hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo f g = g . fmap (hylo f g) . f

-- Map [ 1,            to: [ Span 1 1,
--       7, 8, 9,            Span 7 3,
--       2, 3, 4 ]           Span 2 3 ]
prepare :: [Int] -> [Span Int]
prepare = reverse . foldl f []
  where f [] n      = [Span n 1]
        f (Span a len : spans) n
          | n == a+len  = Span a (len+1) : spans
          | otherwise   = Span n 1 : (Span a len : spans)

-- Map [Span 1 1, Span 7 3] to [1, 7, 8, 9]
expand :: [Span Int] -> [Int]
expand = concatMap extract
  where extract (Span n length) = map (n+) [0 .. length-1]

main = do
  file <- readFile "lgis-small.input"
  let permutation = map read (words $ head $ tail $ lines file)
  let pool = prepare permutation
  let longest = tail $ hylo coalgebra algebra (Span 0 0, pool)
  print $ expand longest

