{- Rosalind

   Code:    PMCH
   Problem: Perfect Matchings and RNA Secondary Structures
   URL:     http://rosalind.info/problems/pmch/
-}

{-# LANGUAGE DeriveFunctor #-}

import Bioinformatics (fastaSeq, parseFASTArna)
import Data.List      (delete)

type Node     = (Int, Char)
type Edge     = (Node, Node)
type Pool     = [Node]
type Matching = [Edge]

data TreeF a = NodeF Edge [a]
               deriving (Show, Functor)

type Coalgebra f a = a -> f a
type Algebra   f a = f a -> a

coalg :: Coalgebra TreeF (Edge, Pool)
coalg (edge, []  ) = NodeF edge []
coalg (edge, pool) = NodeF edge subs
  where 
    subs = let n1 = head pool
           in  [ ((n1, n2), rest) | n2 <- filter (isComplement n1) pool,
                                    let rest = delete n1 $ delete n2 pool ]

alg :: Algebra TreeF (Edge, [Matching])
--alg (NodeF e@((0,_),_) ms  ) = (e, ms)
alg (NodeF edge []       ) = (edge, [[edge]])
alg (NodeF edge matchings) = (edge, concat [ push edge ms | (_, ms) <- matchings ])
    where
      push :: Edge -> [Matching] -> [Matching]
      push edge [] = [[edge]]
      push edge ms = [ edge : es | es <- ms ]

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

isComplement :: Node -> (Node -> Bool)
isComplement (_, 'A') = \(_, n) -> n == 'U'
isComplement (_, 'U') = \(_, n) -> n == 'A'
isComplement (_, 'C') = \(_, n) -> n == 'G'
isComplement (_, 'G') = \(_, n) -> n == 'C'

type RNA = [Char]

prepare :: RNA -> Pool
prepare rna = zip [1..] rna

process :: RNA -> [Matching]
process rna = let pool = prepare rna
                  edge = ((0, '_'), (0, '_'))
                  folded = hylo alg coalg (edge, pool)
              in  snd folded

main = do
  file <- readFile "pmch-sample.input"
  let rna = fastaSeq $ head $ parseFASTArna file
  let matchings = process rna
  print $ length matchings

