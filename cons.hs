{- Rosalind

   Code:    CONS
   Problem: Consensus and Profile
   URL:     http://rosalind.info/problems/cons/
-}

import Data.List      (intercalate, maximumBy)
import Data.Ord       (comparing)
import Bioinformatics (fastaDNA, parseFASTA)

type Nucleotide = Char

-- Number of nucleotides at a certain position across a set of DNA strands
type Profile = [(Nucleotide, Int)]

-- Count this nucleotide at this position
add :: Nucleotide -> Profile -> Profile
add nucleo [] = [(nucleo, 1)]
add nucleo ( (n,i) : ns )
  | n == nucleo = (n, i+1) : ns
  | otherwise   = (n, i  ) : add nucleo ns

get :: Nucleotide -> Profile -> Int
get nucleo [] = 0
get nucleo ( (n,i) : ns)
  | n == nucleo = i
  | otherwise   = get nucleo ns

profile :: [String] -> [Profile]
profile ([]:_) = []
profile dnas   = these : profile rest
  where
    these   = foldr add [] nucleos
    nucleos = map head dnas
    rest    = map tail dnas

-- Find the first consensus string (no specific secondary ordering if 
-- nucleotide counts match)
consensus :: [Profile] -> String
consensus []     = ""
consensus (p:ps) = maxLetter p : consensus ps
  where
    maxLetter = fst . maximumBy (comparing snd)

-- Show the profile summary for a single nucleotide
showProfile :: [Profile] -> Nucleotide -> String
showProfile ps n = [n] ++ ": " ++ counts
  where
    counts = intercalate " " $ map (show . get n) ps

main = do
  file <- readFile "cons.input"
  let parsed = parseFASTA file
  let profiled = profile (map fastaDNA parsed)
  print $ consensus profiled
  print $ showProfile profiled 'A'
  print $ showProfile profiled 'C'
  print $ showProfile profiled 'G'
  print $ showProfile profiled 'T'

