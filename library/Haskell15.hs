module Haskell15 (repli) where

repli :: [a] -> Int -> [a]
repli a n = concatMap (replicate n) a