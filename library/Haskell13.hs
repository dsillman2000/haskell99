module Haskell13 (encodeDirect) where

import Haskell11 (Run(..), encodeModified)

encodeDirect :: Eq a => [a] -> [Run a]
encodeDirect = encodeModified