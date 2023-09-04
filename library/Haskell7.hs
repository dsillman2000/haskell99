module Haskell7 (NestedList(..), flatten) where

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List as) = concatMap flatten as