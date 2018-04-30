{- Rosalind

   Code:    SETO
   Problem: Introduction to Set Operations
   URL:     http://rosalind.info/problems/seto/
-}

import Data.List (intercalate)
import Data.Set  (Set, fromList, toList, difference, intersection, union)

-- Map "{1, 2, 3}" to [1, 2, 3]
toSet :: String -> Set Int
toSet ss = fromList $ map read $ words digits
  where digits = filter (`notElem` "{},") ss

printSet :: Set Int -> String
printSet set = "{" ++ csv ++ "}"
  where csv = intercalate ", " (map show $ toList set)

main = do
  file <- readFile "seto.input"
  let ls = lines file
  let n = (read $ head ls) :: Int
  let set1 = toSet $ ls !! 1
  let set2 = toSet $ ls !! 2
  
  putStrLn $ printSet $ union        set1 set2
  putStrLn $ printSet $ intersection set1 set2
  putStrLn $ printSet $ difference   set1 set2
  putStrLn $ printSet $ difference   set2 set1

  -- Set complements
  putStrLn $ printSet $ difference (fromList [1..n]) set1
  putStrLn $ printSet $ difference (fromList [1..n]) set2

