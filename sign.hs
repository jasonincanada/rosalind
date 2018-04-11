{- Rosalind

   Code:    SIGN
   Problem: Enumerating Oriented Gene Orderings
   URL:     http://rosalind.info/problems/sign/
-}

import Data.Bits ((.&.))
import Data.List (permutations)

-- For n = 2, this generates [ [1,1], [1,-1], [-1,1], [-1,-1] ]
signs :: Int -> [[Int]]
signs n = [ gen n num | num <- nums ]
  where
    nums = [1..2^n] :: [Int]

    gen 0 num = []
    gen i num = let sign = if num .&. (2^(i-1)) > 0
                           then 1
                           else (-1)
                in  sign : gen (i-1) num

process :: Int -> (Int, [[Int]])
process n = (count, perms)
  where
    count = product [1..n] * 2^n
    perms = [ zipWith (*) ps ss | ps <- permutations [1..n],
                                  ss <- signs n ]

main = do
  let n = 6
  let (count, perms) = process n
  print count
  mapM_ (putStrLn . unwords . map show) perms

