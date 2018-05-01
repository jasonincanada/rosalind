{- Rosalind

   Code:    DBRU
   Problem: Constructing a De Bruijn Graph
   URL:     http://rosalind.info/problems/dbru/
-}

import Data.List (nub, sort)

type DNA = [Char]
type Digraph a = [(a, a)]

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

-- Construct the De Bruijn graph for these DNA k-mers
graph :: [DNA] -> Digraph DNA
graph dnas = map (\d -> (init d, tail d)) dnas

printed :: (DNA, DNA) -> String
printed (n1, n2) = "(" ++ n1 ++ ", " ++ n2 ++ ")"

main = do
  file <- readFile "dbru.input"
  let mers = nub $ lines file
  let revcomps = map (reverse . map complement) mers
  let combined = sort $ nub $ mers ++ revcomps
  let graphed = graph combined
  mapM_ (putStrLn . printed) graphed

