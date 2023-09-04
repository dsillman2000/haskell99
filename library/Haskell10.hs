module Haskell10 (encode) where


encode :: Eq a => [a] -> [(a, Int)]
encode = foldr func []
    where
    func e a
      | null a              = [(e, 1)]
      | (fst . head) a == e = (e, (succ . snd . head) a):tail a
      | otherwise           = (e, 1):a