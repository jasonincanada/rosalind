module Common (overlap50) where

import Data.List (isPrefixOf, isSuffixOf)

-- Find all overlaps of a new list with a base one, considering overlaps
-- of length at least half of the new list
overlap50 :: Eq a => [a] -> [a] -> [[a]]
overlap50 base addition = 
  let len = length addition
      minLen = (len+1) `div` 2
      span = [minLen .. len-1]
  in  [ base ++ (drop i addition) | i <- span,
                                    (take i addition) `isSuffixOf` base ]
      ++ 
      [ take (len-i) addition ++ base | i <- span,
                                        (drop (len-i) addition) `isPrefixOf` base ]

