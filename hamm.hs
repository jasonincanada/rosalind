{- Rosalind

   Code:    HAMM
   Problem: Counting Point Mutations
   URL:     http://rosalind.info/problems/hamm/
-}

hamming :: String -> String -> Maybe Int
hamming xs ys 
  | length xs /= length ys = Nothing
  | otherwise              = Just $ length different
  where 
    different = filter (uncurry (/=)) zipped
    zipped    = zip xs ys

main = do
  file <- readFile "hamm.input"
  let dna1 = lines file !! 0
  let dna2 = lines file !! 1
  print $ hamming dna1 dna2

