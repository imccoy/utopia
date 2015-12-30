module Html where

import Prelude hiding (head, div)

import Control.Lens
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import Diff (Mapping, mappingDst, mappingSrc, mappingChildren)
import DiffTree hiding (name, label)
import qualified DiffTree

mappingHtml :: SrcNode -> Mapping -> Html
mappingHtml srcNode mapping = do
  docType
  head $ do
    link ! rel "stylesheet" ! href "styles.css"
    script ! src "script.js" $ text ""
  body $ do
    table $
      tr $ do
        td ! width "50%" $ srcMapping srcNode
        td ! width "50%" $ dstMapping mapping

srcMapping :: SrcNode -> Html
srcMapping node = div ! class_ "src-node" $ do
  div ! class_ "src-node__label" $ do
    a ! name (stringValue $ show $ node ^. srcNodeId) $ do
      text $ node ^. diffTree . DiffTree.label
      text " "
      text $ node ^. diffTree . DiffTree.name . non ""
  div ! class_ "src-node__children" $
    forM_ (node ^. srcNodeChildren) srcMapping

dstMapping :: Mapping -> Html
dstMapping mapping = div ! class_ "dst-node" $ do
  div ! class_ "dst-node__label" $ do
    a ! name (stringValue $ show $ mapping ^. mappingDst . dstNodeId) ! href (dstMappingLink mapping) $ do
      text $ mapping ^. mappingDst . diffTree . DiffTree.label
      text " "
      text $ mapping ^. mappingDst . diffTree . DiffTree.name . non ""
  div ! class_ "dst-node_children" $
    forM_ (mapping ^. mappingChildren) dstMapping

dstMappingLink mapping = case mapping ^. mappingSrc of
  Just srcNode -> stringValue $ "#" ++ (show $ srcNode ^. srcNodeId)
  Nothing -> mempty
