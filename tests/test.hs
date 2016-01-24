import Test.Tasty

import qualified Diffs

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Diffs.tests]
