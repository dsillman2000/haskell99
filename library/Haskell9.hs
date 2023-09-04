module Haskell9 (pack) where

pack :: Eq a => [a] -> [[a]]
pack = foldr func []
    where
    func e a = 
        if not (null a) && e == head (head a)
        then (e : head a) : tail a
        else [e]:a