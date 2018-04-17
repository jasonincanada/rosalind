{- Rosalind

   Code:    LONG
   Problem: Genome Assembly as Shortest Superstring
   URL:     http://rosalind.info/problems/long/
-}

{-# LANGUAGE DeriveFunctor #-}

import Bioinformatics (fastaSeq, parseFASTAdna)
import Common         (overlap50)
import Data.List      (delete)

type Strand = String
type Pool   = [Strand]

data TreeF a = NodeF Int Strand [a]
               deriving (Functor)

newtype Fix f = Fix { unFix :: f (Fix f) }

type Coalgebra f a = a -> f a
type Algebra   f a = f a -> a

coalg :: Coalgebra TreeF (Int, Strand, Pool)
coalg (count, strand, pool) = NodeF count strand subtrees
  where 
    subtrees = [ (count+1, overlapped, delete s pool) 
                  | s <- pool, 
                    overlapped <- overlap50 strand s ]

alg :: Algebra TreeF [(Int, Strand)]
alg (NodeF count strand []) = [(count, strand)]
alg (NodeF _     _      ss) = concat ss

hylo :: Functor f => Coalgebra f b -> Algebra f a -> b -> a
hylo g f = f . fmap (hylo g f) . g

main = do
  file <- readFile "long.input"
  let snippets = map fastaSeq $ parseFASTAdna file
  let count = length snippets
  let strands = hylo coalg alg (1, head snippets, tail snippets)
  let candidates = filter ((==count) . fst) strands
  print $ snd $ head $ candidates

