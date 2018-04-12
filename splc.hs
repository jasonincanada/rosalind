{- Rosalind

   Code:    SPLC
   Problem: RNA Splicing
   URL:     http://rosalind.info/problems/splc/
-}

import Data.List (isPrefixOf)
import Bioinformatics (fastaSeq, parseFASTAdna)
import Codons (codonSeqToAminos)

type DNA     = String
type Intron  = String

splice :: DNA -> Intron -> DNA
splice dna intron = go dna
  where go dna
          | null dna                = []
          | intron `isPrefixOf` dna = go $ drop (length intron) dna
          | otherwise               = head dna : go (tail dna)

deletes :: DNA -> [Intron] -> DNA
deletes = foldl splice

transcribe :: String -> String
transcribe = map f
  where f 'T' = 'U'
        f  x  =  x

translate = codonSeqToAminos

process :: DNA -> [Intron] -> String
process dna introns = translate . transcribe $ deletes dna introns

main = do
  file <- readFile "splc.input"
  let parsed = map fastaSeq $ parseFASTAdna file
  let dna = head parsed
  let introns = tail parsed
  print $ process dna introns

