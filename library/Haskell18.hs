module Haskell18 (slice) where

import Haskell17 (split)

slice :: [a] -> Int -> Int -> [a]
slice as lo hi = fst (split (snd (split as (pred lo))) (hi - lo))