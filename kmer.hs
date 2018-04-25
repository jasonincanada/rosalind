{- Rosalind

   Code:    KMER
   Problem: k-Mer Composition
   URL:     http://rosalind.info/problems/kmer/
-}

import qualified Data.Map as M
import           Data.Maybe     (fromMaybe)
import           Bioinformatics (fastaSeq, parseFASTAdna)
import           Common         (allSubs)

add :: String -> M.Map String Int -> M.Map String Int
add s map = case M.lookup s map of
              Nothing -> M.insert s 1 map
              Just n  -> M.insert s (n+1) map

composition :: [Char] -> Int -> String -> [Int]
composition letters k dna = let subs   = allSubs 4 dna
                                counts = foldr add M.empty subs
                            in  map (\mer -> fromMaybe 0 (M.lookup mer counts)) (mers 4)

-- Construct all DNA k-mers in lexicographic order
mers :: Int -> [String]
mers 0 = [""]
mers k = concat [ map (d:) $ mers (k-1) | d <- "ACGT" ]

main = do
  file <- readFile "kmer.input"
  let dna = fastaSeq $ head $ parseFASTAdna file
  let comp = composition "ACGT" 4 dna
  putStrLn $ unwords . map show $ comp

