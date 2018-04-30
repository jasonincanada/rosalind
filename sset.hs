{- Rosalind

   Code:    SSET
   Problem: Counting Subsets
   URL:     http://rosalind.info/problems/sset/
-}

-- Count the number of subsets of a set
subsets :: Integral a => a -> a
subsets 0 = 1
subsets n = 2 * subsets (n-1) `mod` 1000000

main = do
  let n = 905
  print $ subsets n

