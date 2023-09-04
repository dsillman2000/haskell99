module Haskell19 (rotate) where

import Haskell17 (split)

rotate :: [a] -> Int -> [a]
rotate as n =
    s2 ++ s1
    where 
        s1 = fst s
        s2 = snd s
        s = split as (n `mod` length as)