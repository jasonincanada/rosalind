{- Rosalind

   Problem: DNA - Counting DNA Nucleotides
   URL:     http://rosalind.info/problems/dna/
-}

import Data.List (group, sort)

process :: String -> [Int]
process = map length . group . sort

main = do
  file <- readFile "dna.input"
  let input = head $ lines file
  print $ process input
  
