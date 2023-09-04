module Haskell6 (isPalindrome) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = reverse l == l