{- Rosalind

   Problem: PROT - Translate an RNA sequence into amino acids
   URL:     http://rosalind.info/problems/prot/
-}

import Data.List       (find)
import Data.List.Split (chunksOf)

process :: [(String, Char)] -> String -> String
process aminos rna = let chunks = chunksOf 3 rna
                     in  concatMap f chunks
  where
     f chunk = case find ((==chunk) . fst) aminos of
                 Nothing     -> ""
                 Just (_, c) -> [c]
                     

-- Parse our list of codon->amino pairs, ignoring Stop codons
toMap :: [String] -> [(String, Char)]
toMap = map m . filter noStop
  where 
    m str    = let tokens = words str
               in  (tokens !! 0, head $ tokens !! 1)
    noStop l = (words l !! 1) /= "Stop"

main = do
  codons <- readFile "codons.txt"
  file   <- readFile "prot.input"
  let map = toMap $ lines codons
  let input = head $ lines file
  print $ process map input 
  
