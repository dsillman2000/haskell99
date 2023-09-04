module Utils (assertErrors) where
import Test.Tasty.HUnit (Assertion, assertFailure)
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless)

assertErrors :: String -> IO t0 -> Assertion
assertErrors s t0 = do
    errored <- catch (t0 >> pure False) handler
    unless errored $ assertFailure s
    where
        handler :: ErrorCall -> IO Bool
        handler _ = pure True