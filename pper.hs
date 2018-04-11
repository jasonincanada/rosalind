{- Rosalind

   Code:    PPER
   Problem: Partial Permutations
   URL:     http://rosalind.info/problems/pper/
-}

perms :: Integer -> Integer -> Integer -> Integer
perms n k m = num `div` denom `mod` m
  where
    num         = factorial n
    denom       = factorial (n-k)
    factorial x = product [2..x]

main = do
  let n = 81
  let k = 8
  print $ perms n k 1000000

