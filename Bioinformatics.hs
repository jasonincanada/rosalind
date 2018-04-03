{- Bioinformatics.hs

   About:  Common code for Rosalind challenges
   Author: Jason Hooper
-}

module Bioinformatics (FASTA(FASTA), fastaID, parseFASTA, fastaDNA) where

import Control.Applicative (many, some)
import NanoParsec

-- A FASTA record is an ID with a DNA sequence
data FASTA = FASTA String String
             deriving (Show)

fastaID :: FASTA -> String
fastaID (FASTA id _) = id

fastaDNA :: FASTA -> String
fastaDNA (FASTA _ dna) = dna

{- Parsing -}

dna :: Parser String
dna = some $ oneOf "ACGT"

newline :: Parser (String -> String -> String)
newline = char '\n' >> return (++)

linesOfDNA :: Parser String
linesOfDNA = chainl dna newline []

-- Parse ">Rosalind_1234" to "Rosalind_1234"
rosalindID :: Parser String
rosalindID = do
  char '>'
  rosalind <- string "Rosalind_"
  id       <- many digit
  return $ rosalind ++ id

-- Parse a FASTA id and possibly many lines of DNA
fasta :: Parser FASTA
fasta = do
  id <- rosalindID
  char '\n'
  dna <- linesOfDNA
  many (char '\n')
  return $ FASTA id dna

-- Parse a file of FASTA data
fastas :: Parser [FASTA]
fastas = many fasta

parseFASTA :: String -> [FASTA]
parseFASTA s = run fastas s

