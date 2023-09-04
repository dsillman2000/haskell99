module Haskell5 (myReverse) where

myReverse :: [a] -> [a]
myReverse = foldr (\e a -> a ++ [e]) []