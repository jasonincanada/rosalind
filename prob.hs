{- Rosalind

   Code:    PROB
   Problem: Introduction to Random Strings
   URL:     http://rosalind.info/problems/prob/
-}

-- Find the log of the probability that a GC-content will exactly match a DNA string
prob :: String -> Float -> Float
prob ns gc = foldr f 0 ns
  where f n acc = let p = if n `elem` "CG"
                          then gc / 2
                          else (1-gc) / 2
                  in  acc + logBase 10 p

process :: String -> [Float] -> [Float]
process s = map (prob s)

main = do
  let s = "GACTCTCGGGCAAAGTAAGGTTTAGGGCAATAAGATGACCTACCGCACCCCACGCTGCTGGTTAAGCTTGTGTCCGTTCAAGTGC"
  let as = "0.107 0.155 0.188 0.282 0.327 0.369 0.450 0.507 0.580 0.629 0.658 0.743 0.800 0.871 0.918"
  let fs = (map read $ words as) :: [Float]
  print $ unwords $ map show $ process s fs

