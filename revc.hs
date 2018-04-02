{- Rosalind

   Problem: REVC - Complementing a Strand of DNA
   URL:     http://rosalind.info/problems/revc/
-}

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

process :: String -> String
process = reverse . map complement

main = do
  file <- readFile "revc.input"
  let input = head $ lines file
  print $ process input
  
