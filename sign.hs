{- Rosalind

   Code:    SIGN
   Problem: Enumerating Oriented Gene Orderings
   URL:     http://rosalind.info/problems/sign/
-}

import Data.List (permutations)

-- For n = 2, this generates [ [1,1], [1,-1], [-1,1], [-1,-1] ]
signs :: Int -> [[Int]]
signs 0 = [[]]
signs n = [ i : rest | i    <- [1, -1],
                       rest <- signs (n-1) ]

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

