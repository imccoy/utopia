module Diff (diff, DiffTree(..), Mapping, mappingDst, mappingSrc, mappingCost, mappingChildren, ReverseMapping, reverseMappingSrc, reverseMappingDsts, reverseMappingChildren, reverseMapping, zeroCostMappings) where

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
import Data.Map (Map)
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

scoreMapping :: DstNode -> SrcNode -> [Mapping] -> Maybe Int
scoreMapping dst src childMappings = do renameCost <- renameCostFor (src ^. diffTree . label) (src ^. diffTree . name) (dst ^. diffTree . label) (dst ^. diffTree . name)
                                        pure $ numUnmappedChildren * childInDstNotMappedCost + numDroppedSrcChildren * childInSrcNotHereCost + mappedChildrenCost + renameCost
  where 
        (unmappedChildren, mappedChildren) = partition (isNothing . _mappingSrc) childMappings
        numUnmappedChildren = length unmappedChildren
        mappedChildrenCost = sum $ map _mappingCost mappedChildren
        numDroppedSrcChildren = let srcChildIds = map (^. srcNodeId) (src ^. srcNodeChildren)
                                    childMappingSrcIds = map (^. srcNodeId) $ catMaybes $ map (^. mappingSrc) childMappings
                                 in length $ srcChildIds \\ childMappingSrcIds

findMatchingNode :: DstNode -> [Mapping] -> SrcNode -> Mapping
findMatchingNode dst dstChildMappings src = let scored = catMaybes [ (srcNode,) <$> scoreMapping dst srcNode dstChildMappings | srcNode <- transitiveClosure (^. srcNodeChildren) src]
                                             in case scored of
                                               [] -> Mapping dst Nothing inventedNodeCost dstChildMappings
                                               _ -> let (srcNode, score) = minimumBy (\a b -> compare (snd a) (snd b)) scored
                                                     in Mapping dst (Just srcNode) score dstChildMappings



matchTrees :: SrcNode -> DstNode -> Mapping
matchTrees src = matchTrees'
  where matchTrees' dst = let dstChildMappings = map (matchTrees src) (dst ^. dstNodeChildren)
                           in findMatchingNode dst dstChildMappings src

diff :: DiffTree -> DiffTree -> Mapping
diff src dst = matchTrees (SrcNode src) (DstNode dst)

zeroCostMappings :: Mapping -> Map SrcNodeId DstNodeId
zeroCostMappings m = case (m ^. mappingCost, m ^. mappingSrc) of
                       (0, Just src) -> Map.singleton (src ^. srcNodeId) (m ^. mappingDst . dstNodeId)
                       _ -> Map.unions $ map zeroCostMappings (m ^. mappingChildren)

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
