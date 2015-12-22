module Diffs where

import Control.Lens
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit

import DiffTree hiding (diffTree)
import Diff (Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren)
import qualified Diff
import Lam

mappingChild n = _Just . mappingChildren . ix n

mappingSrcNodeId :: Getter Mapping T.Text
mappingSrcNodeId = mappingSrc . DiffTree.srcNodeId . (to $ T.pack . show)

tests :: TestTree
tests = testGroup "DiffTree" 
  [ testCase "simple ordering change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Lit" (Just "2") []
                , DiffTree "v1b" "Lit" (Just "4") []
                ]
          v2 =  DiffTree "M" "Module" Nothing $
                [ DiffTree "v2b" "Lit" (Just "4") []
                , DiffTree "v2a" "Lit" (Just "2") []
                ]
          diffResult = Diff.diff v1 v2
       in do (Just "v1b") `compare` (diffResult ^? mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1a") `compare` (diffResult ^? mappingChild 1 . _Just . mappingSrcNodeId) @?= EQ
  , testCase "simple value swap" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                , DiffTree "v1b" "Binding" (Just "Gregory") $
                  [ DiffTree "v1b.value" "Lit" (Just "Shiver me timbers") [] ]
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") $
                  [ DiffTree "v2b.value" "Lit" (Just "Shiver me timbers") [] ]
                , DiffTree "v2b" "Binding" (Just "Gregory") $
                  [ DiffTree "v2a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                ]
          diffResult = Diff.diff v1 v2
       in do 
             (Just "v1a") `compare` (diffResult ^? mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1b.value") `compare` (diffResult ^? mappingChild 0 . mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1b") `compare` (diffResult ^? mappingChild 1 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1a.value") `compare` (diffResult ^? mappingChild 1 . mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
  , testCase "ordering change with value change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Lit" (Just "mate") []
                , DiffTree "v1b" "Lit" (Just "nine") []
                ]
          v2 =  DiffTree "M" "Module" Nothing $
                [ DiffTree "v2b" "Lit" (Just "none") [] -- should map to nine, aka v1b
                , DiffTree "v2a" "Lit" (Just "mute") [] -- should map to mate, aka v1a
                ]
          diffResult = Diff.diff v1 v2
       in do (Just "v1b") `compare` (diffResult ^? mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1a") `compare` (diffResult ^? mappingChild 1 . _Just . mappingSrcNodeId) @?= EQ
  , testCase "simple value swap with value change" $
      let v1 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v1a" "Binding" (Just "Frederick") $
                  [ DiffTree "v1a.value" "Lit" (Just "Aye Aye, captain") [] ]
                , DiffTree "v1b" "Binding" (Just "Gregory") $
                  [ DiffTree "v1b.value" "Lit" (Just "Shiver me tumblr") [] ]
                ]
          v2 = DiffTree "M" "Module" Nothing $
                [ DiffTree "v2a" "Binding" (Just "Frederick") $
                  [ DiffTree "v2b.value" "Lit" (Just "Shiver me timbers") [] ]
                , DiffTree "v2b" "Binding" (Just "Gregory") $
                  [ DiffTree "v2a.value" "Lit" (Just "Aye Aye, cap'n") [] ]
                ]
          diffResult = Diff.diff v1 v2
       in do 
             (Just "v1a") `compare` (diffResult ^? mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1b.value") `compare` (diffResult ^? mappingChild 0 . mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1b") `compare` (diffResult ^? mappingChild 1 . _Just . mappingSrcNodeId) @?= EQ
             (Just "v1a.value") `compare` (diffResult ^? mappingChild 1 . mappingChild 0 . _Just . mappingSrcNodeId) @?= EQ
  ]
