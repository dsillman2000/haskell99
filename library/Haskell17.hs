module Haskell17 (split) where

split :: [a] -> Int -> ([a], [a])
split as n =
    (
        fmap snd (filter ((<=n) . fst) (zip [1..] as)), 
        fmap snd (filter ((>n) . fst) (zip [1..] as))
    )