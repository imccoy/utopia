module Html where

import Prelude hiding (head, div)

import Control.Lens hiding (children, mapping)
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import Diff (Mapping, mappingDst, mappingSrc, mappingChildren, ReverseMapping, reverseMappingSrc, reverseMappingDsts, reverseMappingChildren)
import DiffTree hiding (name, label)
import qualified DiffTree

mappingHtml :: ReverseMapping -> Mapping -> Html
mappingHtml reverseMapping mapping = do
  docType
  head $ do
    link ! rel "stylesheet" ! href "styles.css"
    script ! src "script.js" $ text ""
  body $ do
    table $
      tr $ do
        td ! width "50%" $ srcMapping reverseMapping
        td ! width "50%" $ dstMapping mapping

srcMapping :: ReverseMapping -> Html
srcMapping reverseMapping = div ! class_ "src-node" $ do
  div ! class_ "src-node__label" $ do
    a ! name (stringValue $ show $ reverseMapping ^. reverseMappingSrc . srcNodeId) $ do
      text $ reverseMapping ^. reverseMappingSrc . diffTree . DiffTree.label
      text " "
      text $ reverseMapping ^. reverseMappingSrc . diffTree . DiffTree.name . non ""
    forM_ (reverseMapping ^. reverseMappingDsts) $ \(dst, _cost) -> do
      text " "
      a ! href (stringValue $ "#" ++ (show $ dst ^. dstNodeId)) $ do
        text "*"
  div ! class_ "src-node__children" $
    forM_ (reverseMapping ^. reverseMappingChildren) srcMapping

dstMapping :: Mapping -> Html
dstMapping mapping = div ! class_ "dst-node" $ do
  div ! class_ "dst-node__label" $ do
    a ! name (stringValue $ show $ mapping ^. mappingDst . dstNodeId) ! href (dstMappingLink mapping) $ do
      text $ mapping ^. mappingDst . diffTree . DiffTree.label
      text " "
      text $ mapping ^. mappingDst . diffTree . DiffTree.name . non ""
  div ! class_ "dst-node_children" $
    forM_ (mapping ^. mappingChildren) dstMapping

dstMappingLink :: Mapping -> AttributeValue
dstMappingLink mapping = case mapping ^. mappingSrc of
  Just srcNode -> stringValue $ "#" ++ (show $ srcNode ^. srcNodeId)
  Nothing -> mempty
