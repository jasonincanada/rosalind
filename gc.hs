{- Rosalind

   Code:    GC
   Problem: Computing GC Content
   URL:     http://rosalind.info/problems/gc/
-}

import Data.List           (maximumBy)
import Data.Ord            (comparing)
import Control.Applicative (many, some)
import NanoParsec

-- A FASTA record is an ID with a DNA sequence
data FASTA = FASTA String String
             deriving (Show)

dna :: Parser String
dna = some $ oneOf "ACGT"

newline :: Parser (String -> String -> String)
newline = char '\n' >> return (++)

linesOfDNA :: Parser String
linesOfDNA = chainl dna newline []

rosalindID :: Parser String
rosalindID = do
  char '>'
  rosalind <- string "Rosalind_"
  id       <- many digit
  return $ rosalind ++ id

fasta :: Parser FASTA
fasta = do
  id <- rosalindID
  char '\n'
  dna <- linesOfDNA
  many (char '\n')
  return $ FASTA id dna

fastas :: Parser [FASTA]
fastas = many fasta

-- Return the percentage of a DNA string that consists of C or G nucleotides
gcContent :: String -> Float
gcContent [] = 0
gcContent ns = num / denom
  where num   = fromIntegral $ length . filter (flip elem "CG") $ ns
        denom = fromIntegral $ length ns

gcpct :: FASTA -> Float
gcpct (FASTA _ dna) = gcContent dna

fastaID :: FASTA -> String
fastaID (FASTA id _) = id

main = do
  file <- readFile "gc.input"
  let parsed = run fastas file
  let max = maximumBy (comparing gcpct) parsed
  print $ (fastaID max, 100 * gcpct max)

