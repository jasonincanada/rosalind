{- Rosalind

   Problem: RNA - Transcribing DNA into RNA
   URL:     http://rosalind.info/problems/rna/
-}

process :: String -> String
process = map f
  where f 'T' = 'U'
        f  x  =  x

main = do
  file <- readFile "rna.input"
  let input = head $ lines file
  print $ process input
  
