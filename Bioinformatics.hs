{- Bioinformatics.hs

   About:  Common code for Rosalind challenges
   Author: Jason Hooper
-}

module Bioinformatics (FASTA(FASTA), fastaID, fastaSeq, parseFASTAaminos, parseFASTAdna) where

import Control.Applicative (many, some, (<|>))
import NanoParsec

-- A FASTA record is an ID with a DNA/RNA/Amino sequence
data FASTA = FASTA String String
             deriving (Show)

fastaID :: FASTA -> String
fastaID (FASTA id _) = id

fastaSeq :: FASTA -> String
fastaSeq (FASTA _ seq) = seq

fastaSequence :: FASTA -> String
fastaSequence (FASTA _ seq) = seq

{- Parsing -}

dna, rna, aminos :: Parser String
dna    = some $ oneOf "ACGT"
rna    = some $ oneOf "ACGU"
aminos = some $ oneOf "ACDEFGHIKLMNPQRSTVWY"

newline :: Parser (String -> String -> String)
newline = char '\n' >> return (++)

linesOfDNA, linesOfRNA, linesOfAminos :: Parser String
linesOfDNA    = linesOf dna
linesOfRNA    = linesOf rna
linesOfAminos = linesOf aminos

linesOf :: Parser String -> Parser String
linesOf p = do
  segments <- many (p <|> string "\n")
  return $ concat $ filter (/="\n") segments

untilNewline :: Parser String
untilNewline = many (satisfy (/='\n'))

-- Parse ">Rosalind_1234" to "Rosalind_1234"
fastaLabel :: Parser String
fastaLabel = do
  char '>'
  label <- untilNewline
  return label

-- Parse a FASTA id and possibly many lines of DNA
fasta :: Parser String -> Parser FASTA
fasta p = do
  label <- fastaLabel
  char '\n'
  sequence <- linesOf p
  return $ FASTA label sequence

-- Parse a file of FASTA data
fastas :: Parser String -> Parser [FASTA]
fastas p = many (fasta p)

parseFASTAdna :: String -> [FASTA]
parseFASTAdna s = run (fastas dna) s

parseFASTAaminos :: String -> [FASTA]
parseFASTAaminos s = run (fastas aminos) s

