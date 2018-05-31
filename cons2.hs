{- Rosalind

   Code:    CONS
   Problem: Consensus and Profile
   URL:     http://rosalind.info/problems/cons/

   Remarks: This is a different, somewhat more Haskell-ish solution compared to cons.hs.
            It uses addition of vectors to tally the profile counts.  cons.hs is
            probably a better solution overall, but this one is a result of thinking
            more in terms of transformations between structures instead of writing the
            "Haskell version" of the older imperative methods I would have used before.

            The consensus function is more elaborate this time since it computes ALL
            possible consensus strings from the profile, where cons.hs computed only
            one.
-}

import Bioinformatics (fastaSeq, parseFASTAdna)
import Data.List      (intercalate)

type Nucleotide = Char
type DNA        = [Nucleotide]

-- Vec stores the counts of nucleotides seen in a specific position across all strands
data Vec     = Vec Int Int Int Int
data Profile = Profile [Vec]

instance Show Profile where
  show (Profile vecs) = unlines $ map line "ACGT"
    where line nucleotide = [nucleotide] ++ ": " ++ (intercalate " " $ tallies nucleotide)
          tallies n       = map (show . tally n) vecs

tally :: Nucleotide -> Vec -> Int
tally 'A' (Vec a _ _ _) = a
tally 'C' (Vec _ c _ _) = c
tally 'G' (Vec _ _ g _) = g
tally 'T' (Vec _ _ _ t) = t

vec :: Nucleotide -> Vec
vec 'A' = Vec 1 0 0 0
vec 'C' = Vec 0 1 0 0
vec 'G' = Vec 0 0 1 0
vec 'T' = Vec 0 0 0 1

-- Add one strand to a profile
process :: DNA -> Profile -> Profile
process dna (Profile vecs) = Profile vecs'
  where vecs'       = zipWith combine vecs dna
        combine v d = v +++ vec d

        (+++) :: Vec -> Vec -> Vec
        (Vec a c g t) +++ (Vec a' c' g' t') = Vec (a+a') (c+c') (g+g') (t+t')

-- All possible consensus strings for a profile
consensuses :: Profile -> [DNA]
consensuses (Profile vecs) = go vecs
  where go []         = [""]
        go (vec:rest) = concat [ map (n:) $ go rest | n <- nucleotides vec ]

        nucleotides :: Vec -> [Nucleotide]
        nucleotides vec = let tallies  = map (\n -> (n, tally n vec)) ['A', 'C', 'G', 'T']
                              maxcount = maximum $ map snd $ tallies
                          in  map fst $ filter ((==maxcount) . snd) tallies

-- Start with an empty profile for strands of constant length n
blank :: Int -> Profile
blank n = Profile $ replicate n (Vec 0 0 0 0)

main :: IO ()
main = do
  file <- readFile "cons2.input"
  let parsed = parseFASTAdna file
  let dna = map fastaSeq parsed
  let len = length $ head dna
  let profile = foldr process (blank len) dna
  putStrLn $ head $ consensuses profile
  print $ profile
