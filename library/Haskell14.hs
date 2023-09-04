module Haskell14 (dupli) where

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)