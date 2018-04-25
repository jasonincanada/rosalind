{- Rosalind

   Code:    LEXF
   Problem: Enumerating k-mers Lexicographically
   URL:     http://rosalind.info/problems/lexf/
-}

import Data.List (nub, permutations)

process :: [a] -> Int -> [[a]]
process list k = go k
  where go 0 = [[]]
        go i = concat [ map (x:) $ go (i-1) | x <- list ]

main = do
  let dataset = "A B C D E F"
  let n = 3
  let list = map head $ words dataset
  mapM_ putStrLn (process list n)

