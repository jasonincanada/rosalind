{- Rosalind

   Code:    MPRT
   Problem: Finding a protein motif
   URL:     http://rosalind.info/problems/mprt/
-}

import Data.List      (intercalate)
import Bioinformatics (FASTA(FASTA), fastaSeq, parseFASTAaminos)
import NanoParsec     (Parser, char, oneOf, parse, satisfy)

-- Parser for the N-glycosylation motif
nGlycoMotif :: Parser String
nGlycoMotif = do
  n     <- char 'N'
  notp1 <- satisfy (/='P')
  st    <- oneOf "ST"
  notp2 <- satisfy (/='P')
  return $ [n, notp1, st, notp2]

-- Return the (1-based) indices of a parser's matches in a sequence
findMotif :: Parser String -> String -> [Int]
findMotif parser seq = go 1 seq
  where go _ ""  = []
        go i seq = case parse nGlycoMotif seq of
                     [] ->     go (i+1) (tail seq)
                     _  -> i : go (i+1) (tail seq)

getIndices :: String -> IO [Int]
getIndices id = do
  text <- readFile $ id ++ ".fasta"
  let parsed = parseFASTAaminos text
  return $ findMotif nGlycoMotif (fastaSeq $ head parsed)

prints :: String -> IO ()
prints id = do
  indices <- getIndices id
  if not (null indices)
  then do
        putStrLn id
        putStrLn $ intercalate " " (map show indices)
  else do
        return ()

main = do
  ids <- readFile "mprt.input"
  mapM_ prints (lines ids)

