{- Rosalind

   Code:    CONS
   Problem: Consensus and Profile
   URL:     http://rosalind.info/problems/cons/

   Remarks: This is a different, somewhat more Haskell-ish solution compared to cons.hs.
            It treats vectors and profiles as monoids, with appropriate instances.  cons.hs
            is probably a better solution overall, but this one is a result of thinking
            more in terms of transformations between structures instead of writing the
            "Haskell version" of the older imperative methods I would have used before.

            The consensus function is more elaborate this time since it computes ALL
            possible consensus strings from the profile, where cons.hs computed only
            one.
-}

import Bioinformatics (fastaSeq, parseFASTAdna)
import Data.Monoid    (Monoid, mempty, mappend, (<>))

type Nucleotide = Char
type DNA        = [Nucleotide]

-- Vec stores the counts of nucleotides seen in a specific position across all strands
data Vec     = Vec Int Int Int Int
data Profile = Profile [Vec]

instance Show Profile where
  show (Profile vecs) = unlines $ map line "ACGT"
    where line nucleotide = [nucleotide] ++ ": " ++ unwords (tallies nucleotide)
          tallies n       = map (show . tally n) vecs

instance Monoid Vec where
  mempty = Vec 0 0 0 0
  mappend (Vec a b c d) (Vec a' b' c' d') = Vec (a+a') (b+b') (c+c') (d+d')

instance Monoid Profile where
  mempty = Profile $ repeat (Vec 0 0 0 0)
  mappend (Profile as) (Profile bs) = Profile $ zipWith (<>) as bs

tally :: Nucleotide -> Vec -> Int
tally 'A' (Vec a _ _ _) = a
tally 'C' (Vec _ c _ _) = c
tally 'G' (Vec _ _ g _) = g
tally 'T' (Vec _ _ _ t) = t

toProfile :: DNA -> Profile
toProfile dna = Profile $ map vec dna
  where vec 'A' = Vec 1 0 0 0
        vec 'C' = Vec 0 1 0 0
        vec 'G' = Vec 0 0 1 0
        vec 'T' = Vec 0 0 0 1

-- All possible consensus strings for a profile
consensuses :: Profile -> [DNA]
consensuses (Profile vecs) = go vecs
  where go []         = [""]
        go (vec:rest) = concat [ map (n:) $ go rest | n <- nucleotides vec ]

        nucleotides :: Vec -> [Nucleotide]
        nucleotides vec = let tallies  = map (\n -> (n, tally n vec)) ['A', 'C', 'G', 'T']
                              maxcount = maximum $ map snd tallies
                          in  map fst $ filter ((==maxcount) . snd) tallies

main :: IO ()
main = do
  file <- readFile "cons2.input"
  let parsed = parseFASTAdna file
  let dnas = map fastaSeq parsed
  let profiles = map toProfile dnas
  let profile = foldr (<>) mempty profiles
  putStrLn $ head $ consensuses profile
  print $ profile
