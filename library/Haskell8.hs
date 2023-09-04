module Haskell8 (compress) where

compress :: Eq a => [a] -> [a]
compress = foldr 
    (\e a -> 
        if not (null a) && (e == head a) 
        then a else e:a
    ) []