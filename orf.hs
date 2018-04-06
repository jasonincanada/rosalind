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

codon :: Parser String
codon = do
  c1 <- oneOf "ACGU"
  c2 <- oneOf "ACGU"
  c3 <- oneOf "ACGU"
  let codon = [c1,c2,c3]
  guard $ not $ isStop codon
  return codon

startCodon, stopCodon :: Parser String
startCodon = string "AUG"
stopCodon  = string "UAA" <|> string "UGA" <|> string "UAG"

codonSequence :: Parser String
codonSequence = do
  start <- startCodon
  seq   <- many codon
  _     <- stopCodon
  return $ start ++ concat seq

-- Find all candidate amino acid sequences in an RNA strand
aminoSequences :: RNA -> [String]
aminoSequences []  = []
aminoSequences rna = let rest = aminoSequences $ tail rna
                     in  case parse codonSequence rna of
                           [(seq, _)] -> codonSeqToAminos seq : rest
                           []         -> rest

proteins :: DNA -> [String]
proteins dna = nub $ forward ++ backward
  where
    rna      = toRNA dna
    forward  = aminoSequences rna
    backward = aminoSequences (complement . reverse $ rna)

    toRNA :: DNA -> RNA
    toRNA = map f
      where f 'T' = 'U'
            f  x  =  x

    complement :: RNA -> RNA
    complement = map f
      where  f 'A' = 'U'
             f 'U' = 'A'
             f 'C' = 'G'
             f 'G' = 'C'

main = do
  file <- readFile "orf.input"
  let dna = fastaSeq $ head $ parseFASTAdna file
  print $ proteins dna

