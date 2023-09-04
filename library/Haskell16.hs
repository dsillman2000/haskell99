module Haskell16 (dropEvery) where

dropEvery :: [a] -> Int -> [a]
dropEvery as n = fmap snd ( 
        filter ((/=n) . fst)
            (zip ( cycle [1 .. n] ) as)
    )