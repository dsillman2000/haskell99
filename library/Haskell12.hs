module Haskell12 (decodeModified) where

import Haskell11 (Run(..))

flattenRun :: Run a -> [a]
flattenRun (Single u) = [u]
flattenRun (Multiple c u) = replicate c u

decodeModified :: [Run a] -> [a]
decodeModified = concatMap flattenRun
