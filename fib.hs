{- Rosalind

   Problem: FIB - Rabbits and Recurrence Relations
   URL:     http://rosalind.info/problems/fib/
-}

process :: Int -> Int -> Int
process n k = go n (0, 1)
  where go n (a, b) | n == 0    = a
                    | otherwise = go (n-1) (b, (a*k)+b)

main = do
  let n = 30
  let k = 2
  print $ process n k
  
