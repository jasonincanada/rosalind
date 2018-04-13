{- Rosalind

   Code:    TREE
   Problem: Completing a Tree
   URL:     http://rosalind.info/problems/tree/
-}

-- The minimum number of edges to add to create a tree from the known
-- adjacencies is the number of disconnected trees, plus the number of
-- single nodes, less 1
process :: Eq a => Int -> [(a, a)] -> Int
process n adj = length graphs + singles - 1
  where
    graphs  = foldr tally [] adj 
    known   = sum $ map length graphs
    singles = n - known

    tally :: Eq a => (a, a) -> [[a]] -> [[a]]
    tally (a,b) [] = [[a, b]]
    tally (a,b) (g:gs)
      | a `elem` g = (b : g) : gs
      | b `elem` g = (a : g) : gs
      | otherwise  = g : tally (a,b) gs

tuple :: [a] -> (a, a)
tuple (x:y:_) = (x, y)

main = do
  file <- readFile "tree.input"
  let ls = lines file
  let n = read $ head ls
  let adjacencies = map (tuple . words) (tail ls)
  print $ process n adjacencies

