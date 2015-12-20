import Test.Tasty

import qualified Diffs

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Diffs.tests]
