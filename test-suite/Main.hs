-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.QuickCheck
import TestHaskell10 (tests10)
import TestHaskell20 (tests20)


main :: IO ()
main = do
    Test.Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" 
    [ tests10, tests20 ]
