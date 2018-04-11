{- Rosalind

   Code:    SIGN
   Problem: Enumerating Oriented Gene Orderings
   URL:     http://rosalind.info/problems/sign/
-}

import Data.List (delete)

permute :: Int -> [[Int]]
permute n = perm [1..n]
  where
    perm :: [Int] -> [[Int]]
    perm [] = [[]]
    perm xs = [ i : rest | i    <- xs ++ map negate xs,
                           rest <- perm $ delete (abs i) xs ]

main = do
  let n = 6
  let perms = permute n
  print $ length perms
  mapM_ (putStrLn . unwords . map show) perms

