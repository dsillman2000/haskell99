module Haskell2 (myButLast, myButLastMaybe) where

myButLast :: [a] -> a
myButLast = head . tail . reverse

myButLastMaybe :: [a] -> Maybe a
myButLastMaybe l = if length l < 2 
    then Nothing 
    else Just ((head . tail . reverse) l)