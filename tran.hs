{- Rosalind

   Code:    TRAN
   Problem: Transitions and Transversions
   URL:     http://rosalind.info/problems/tran/
-}

import Bioinformatics (fastaSeq, parseFASTAdna)

type DNA = String

process :: DNA -> DNA -> Double
process s1 s2 = let trans = length $ filter (==True) $ zipWith isTransition s1 s2
                    trasv = length $ filter (==True) $ zipWith isTransversion s1 s2
                in  fromIntegral trans / fromIntegral trasv

isTransition :: Char -> Char -> Bool
isTransition a b = a /= b 
                   && ( isPurine a && isPurine b 
                     || isPyrimidine a && isPyrimidine b)

isTransversion :: Char -> Char -> Bool
isTransversion a b = a /= b 
                     && ( isPurine a && isPyrimidine b
                       || isPurine b && isPyrimidine a)
                      
isPurine     = flip elem "AG"
isPyrimidine = flip elem "CT"

main = do
  file <- readFile "tran.input"
  let strands = map fastaSeq $ parseFASTAdna file
  print $ process (strands !! 0) (strands !! 1)

