module Haskell20 (removeAt) where

import Haskell3 (elementAt)

removeAt :: [a] -> Int -> (a, [a])
removeAt [] _ = error "Cannot remove element from empty list"
removeAt as n = (elementAt as n, map snd (filter ((/=n) . fst) (zip [1..] as)))