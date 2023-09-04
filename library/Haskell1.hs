-- | An example module.
module Haskell1 (myLast, myLastMaybe) where

myLast :: [a] -> a
myLast [] = error "Cannot get end of empty list."
myLast [a] = a
myLast (_:as) = myLast as

myLastMaybe :: [a] -> Maybe a
myLastMaybe [] = Nothing
myLastMaybe [a] = Just a
myLastMaybe (_:as) = myLastMaybe as