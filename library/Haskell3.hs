module Haskell3 (elementAt, elementAtMaybe) where

elementAt :: [a] -> Int -> a
elementAt l i = if i == 1 then head l else elementAt (tail l) (i - 1)

elementAtMaybe :: [a] -> Int -> Maybe a
elementAtMaybe l i
  | null l      = Nothing
  | i == 1      = Just (head l)
  | otherwise   = elementAtMaybe (tail l) (i - 1)