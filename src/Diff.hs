module Diff (diff, DiffTree(..), Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren, ReverseMapping, reverseMappingSrc, reverseMappingDsts, reverseMappingChildren, reverseMapping) where

{-
 - The algorithm here is inspired by the gumtree paper and code:
 -    https://github.com/GumTreeDiff/gumtree
 -    "Fine-grained and accurate source code differencing", Falleri, Morandart, Blanc, Martinez & Monperrus. ASE '14 Proceedings of the 29th ACM/IEEE international conference on Automated software engineering
 -
 - Their stuff is a lot cooler than mine, but I didn't find a way to get the results I wanted from it.
 -
 - Ideas for future work:
 -   * change Mapping to have a `Maybe (SrcNode, Int)` instead of a `Maybe SrcNode` and
 -     an always-present mapping cost. This would allow more sophisticated costs for
 -     invented nodes.
 -   * optimise the costs (both those defined here and the defaultEditCosts from the
 -     levenshtein module)
 -   * work out if this code is weird from a lens idiom perspective.
 -     For example, do we want to do `isNothing . _mappingSrc` or `mappingSrc . to isNothing`
 -     or something else?
 -   * when a subtree occurs in multiple places in src, pick the one that is closest to us, so
 -     when mapping `(n, n)` to `(n, n + 3 * ( n))` we should map src's first n to dst's first
 -     and src's second to dst's second and third.
 -   * favour using each node exactly once?
 -}

import Control.Lens hiding (children)
import Data.List
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Maybe
import Safe (fromJustDef)

import Text.EditDistance

import DiffTree

data Mapping = Mapping { _mappingDst :: DstNode
                       , _mappingSrc :: Maybe SrcNode
                       , _mappingCost :: Int
                       , _mappingChildren :: [Mapping]
                       }
makeLenses ''Mapping

instance Show Mapping where
  show = unlines . printMappings' ""
    where printMappings' prefix m = (printMapping prefix m):(concat $ printChildren (prefix ++ "  ") m)
          printMapping prefix (Mapping d s _ _) = prefix ++ show (d ^. dstNodeId) ++ "<-" ++ show (fmap (^. srcNodeId) s)
          printChildren prefix (Mapping _ _ _ children) = [printMappings' prefix child | child <- children]

transitiveClosure :: (a -> [a]) -> a -> [a]
transitiveClosure f v = v:(concat [transitiveClosure f c | c <- f v])

renameCostFor :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Int
renameCostFor srcLabel (Just srcName) dstLabel (Just dstName)
  | srcLabel == dstLabel    = Just $ levenshteinDistance defaultEditCosts (show srcName) (show dstName)
  | otherwise               = Nothing
renameCostFor srcLabel Nothing dstLabel Nothing
  | srcLabel == dstLabel    = Just 0
  | otherwise               = Nothing
renameCostFor _ _ _ _       = Nothing

-- I totally just made these numbers up
childInDstNotMappedCost :: Int
childInDstNotMappedCost = 10
childInSrcNotHereCost :: Int
childInSrcNotHereCost = 5
inventedNodeCost :: Int
inventedNodeCost = 100 

scoreMapping :: Mapping -> Int
scoreMapping (Mapping _ src renameCost childMappings) = numUnmappedChildren * childInDstNotMappedCost + numDroppedSrcChildren * childInSrcNotHereCost + renameCost
  where numUnmappedChildren = length $ filter (isNothing . _mappingSrc) childMappings
        numDroppedSrcChildren = let srcChildIds = map (^. srcNodeId) (src ^. _Just . srcNodeChildren)
                                    childMappingSrcIds = map (^. srcNodeId) $ catMaybes $ map (^. mappingSrc) childMappings
                                 in length $ srcChildIds \\ childMappingSrcIds

compareMappings :: Mapping -> Mapping -> Ordering
compareMappings a b = compare (scoreMapping a) (scoreMapping b)

findMatchingNode :: DstNode -> [Mapping] -> SrcNode -> Mapping
findMatchingNode dst dstChildMappings = findMatchingNode'
  where findMatchingNode' src = case possibleMappings src of
                                  [] -> Mapping dst Nothing inventedNodeCost dstChildMappings
                                  mappings -> minimumBy compareMappings mappings
        possibleMappings src = catMaybes [ mappingTo srcNode | srcNode <- transitiveClosure (^. srcNodeChildren) src]
        mappingTo src = Mapping dst (Just src) <$> renameCostFor (src ^. diffTree . label) (src ^. diffTree . name) (dst ^. diffTree . label) (dst ^. diffTree . name) <*> pure dstChildMappings

matchTrees :: SrcNode -> DstNode -> Mapping
matchTrees src = matchTrees'
  where matchTrees' dst = let dstChildMappings = map (matchTrees src) (dst ^. dstNodeChildren)
                           in findMatchingNode dst dstChildMappings src

diff :: DiffTree -> DiffTree -> Mapping
diff src dst = matchTrees (SrcNode src) (DstNode dst)

data ReverseMapping = ReverseMapping { _reverseMappingSrc :: SrcNode
                                     , _reverseMappingDsts :: [(DstNode, Int)]
                                     , _reverseMappingChildren :: [ReverseMapping]
                                     }
makeLenses ''ReverseMapping

reverseMapping :: Mapping -> SrcNode -> ReverseMapping
reverseMapping unreversed = go
  where go srcNode = ReverseMapping srcNode 
                                    (fromJustDef [] $ Map.lookup (srcNode ^. srcNodeId) dstNodes)
                                    [go child | child <- srcNode ^. srcNodeChildren]
        dstNodes = Map.fromListWith (++) $ 
                                    [ (src ^. srcNodeId, [(m ^. mappingDst, m ^. mappingCost)])
                                    | m <- transitiveClosure (^. mappingChildren) unreversed
                                    , src <- maybeToList $ m ^. mappingSrc
                                    ]
