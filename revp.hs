{- Rosalind

   Code:    REVP
   Problem: Locating Restriction Sites
   URL:     http://rosalind.info/problems/revp/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'

process :: String -> [(Int, Int)]
process dna = go 1 dna
  where go _ "" = []
        go i dna = concat [ if isPalin (take n dna)
                            then [(i, n)]
                            else [] | n <- [4,6,8,10,12],
                                      n <= length dna ]
                   ++ go (i+1) (tail dna)

        isPalin dna = dna == (reverse . map complement $ dna)

main = do
  file <- readFile "revp.input"
  let dna = fastaSeq $ head $ parseFASTAdna file
  let pairs = process dna
  mapM_ putStrLn $ map (unwords . map show) . map (\(a,b) -> [a, b]) $ pairs

