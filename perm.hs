{- Rosalind

   Code:    PERM
   Problem: Enumerating Gene Orders
   URL:     http://rosalind.info/problems/perm/
-}

import Data.List (permutations, intercalate)

permute :: Int -> (Int, [[Int]])
permute n = (count, perms)
  where
    count = product [1..n]

    -- My solution is to use Haskell's built-in solution >:)
    perms = permutations [1..n]

main = do
  let n = 5
  let (count, perms) = permute n
  print count
  mapM_ (putStrLn . unwords . map show) perms

