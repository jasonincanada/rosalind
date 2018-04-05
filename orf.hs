{- Rosalind

   Code:    ORF
   Problem: Open reading frames
   URL:     http://rosalind.info/problems/orf/
-}

import Control.Applicative ((<|>), many)
import Control.Monad       (guard)
import Data.List           (isPrefixOf, nub)
import Bioinformatics      (fastaSeq, parseFASTAdna)
import Codons              (codonSeqToAminos, isStop)
import NanoParsec          (Parser, string, oneOf, parse)

type DNA = [Char]
type RNA = [Char]

toRNA :: DNA -> RNA
toRNA = map f
  where f 'T' = 'U'
        f  x  =  x

indices :: Eq a => [a] -> [a] -> [Int]
indices []       _    = []
indices haystack needle = go 0 haystack
  where
    go n []  = []
    go n hay = if needle `isPrefixOf` hay
               then n : go (n+1) (tail hay)
               else     go (n+1) (tail hay)

codon :: Parser String
codon = do
  c1 <- oneOf "ACGU"
  c2 <- oneOf "ACGU"
  c3 <- oneOf "ACGU"
  let codon = [c1,c2,c3]
  guard $ not $ isStop codon
  return codon

stopCodon :: Parser String
stopCodon = string "UAA" <|> string "UGA" <|> string "UAG"

codonSequence :: Parser String
codonSequence = do
  codons <- many codon
  _      <- stopCodon
  return $ concat codons

-- Find all candidate proteins from an RNA strand
frames :: RNA -> [String]
frames rna = 
  let starts = indices rna "AUG"
      mapped = map (\index -> let strand = drop index rna
                              in  case parse codonSequence strand of
                                    [(seq, _)] -> codonSeqToAminos seq
                                    []         -> []
                   ) starts
  in  filter (not . null) mapped

complement :: String -> String
complement = map f
  where  f 'A' = 'T'
         f 'T' = 'A'
         f 'C' = 'G'
         f 'G' = 'C'

main = do
  file <- readFile "orf.input"
  let dna = fastaSeq $ head $ parseFASTAdna file
  let fwd = frames $ toRNA dna
  let bwd = frames $ toRNA $ complement . reverse $ dna
  let both = fwd ++ bwd
  let distinct = nub both
  print distinct

