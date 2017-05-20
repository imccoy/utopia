import Test.Tasty

import qualified Diffs
import qualified ParserTest as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Diffs.tests, Parser.tests]
