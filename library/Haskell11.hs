module Haskell11 (Run(..), encodeModified, valRun, countRun) where
import Test.Tasty.QuickCheck
import Control.Monad

data Run g = Single g | Multiple Int g

incRun :: Run a -> Run a
incRun (Single u) = Multiple 2 u
incRun (Multiple c u) = Multiple (succ c) u

valRun :: Run a -> a
valRun (Single u) = u
valRun (Multiple _ u) = u

countRun :: Run a -> Int
countRun (Single _) = 1
countRun (Multiple c _) = c

encodeModified :: Eq a => [a] -> [Run a]
encodeModified = foldr func ([] :: [Run a])
    where func e ac
           | null ac               = [ Single e ]
           | valRun (head ac) == e = incRun (head ac) : tail ac
           | otherwise             = Single e:ac

instance Eq a => Eq (Run a) where
    x == y = (valRun x == valRun y) && (countRun x == countRun y)

instance Show a => Show (Run a) where
    show (Single u)     = "(Single " ++ show u ++ ")" 
    show (Multiple c u) = "(Multiple " ++ show c ++ " " ++ show u ++ ")"

instance Arbitrary a => Arbitrary (Run a) where
  arbitrary = oneof [ fmap Single arbitrary,
                      liftM2 Multiple (fmap ((+1) . abs) arbitrary) arbitrary ]
  shrink (Single _) = []
  shrink (Multiple 2 u) = [Single u]
  shrink (Multiple c u) = [Multiple (pred c) u]