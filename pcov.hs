{- Rosalind

   Code:    PCOV
   Problem: Genome Assembly with Perfect Coverage
   URL:     http://rosalind.info/problems/pcov/

   Remarks: 2 May 2018

            This revision gives the correct answer without needing to
            manually remove the overlap.

            1 May 2018

            This outputs almost the correct answer, but I had to do a text
            search of the beginning of the strand to know where it started
            to overlap at the end so I could manually remove the overlap.
            So there is some room to improve this code so it gives the
            correct answer without the overlap.
-}

{-# LANGUAGE DeriveFunctor #-}

import Data.List (delete, minimumBy)
import Data.Ord  (comparing)

type DNA = [Char]

data TreeF a = NodeF DNA [a]
               deriving (Functor, Show)

type Pool = [DNA]
type Coalgebra f a = a -> f a

coalgebra :: Coalgebra TreeF (DNA, Pool)
coalgebra (dna, []  ) = NodeF dna []
coalgebra (dna, pool) = NodeF dna subs
  where subs = [ (d, delete d pool) | d <- pool,
                                      tail dna == init d ]

type Algebra f a = f a -> a

algebra :: Algebra TreeF [(Int, DNA)]
algebra (NodeF dna []     ) = [ (  1, [last dna]) ]
algebra (NodeF dna strands) = [ (n+1,  last dna : acc) | (n, acc) <- concat strands ]

hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo coalg alg = alg . fmap (hylo coalg alg) . coalg

main = do
  file <- readFile "pcov.2.input"
  let fragments = lines file
  let count = length fragments
  let candidates = hylo coalgebra algebra (head fragments, tail fragments)
  let allstrandsused = filter (\(n, _) -> n == count) candidates
  let smallest = minimumBy (comparing (length . snd)) allstrandsused
  print $ smallest

