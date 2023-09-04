module Haskell4 (myLength) where

myLength :: [a] -> Int
myLength = foldr (\_ a -> succ a) 0