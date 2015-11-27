module Main where

import Debug.Trace

import Prelude hiding (putStrLn)

import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Aeson.Encode
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Lazy.Builder
import qualified Data.UUID as UUID
import qualified Data.UUID.V1
import qualified Data.UUID.V4
import Network.Wreq
import Safe

import qualified Lam
import qualified Code

data CodeTree = CodeTree (Maybe Text) Text [CodeTree]

class ToCodeTree a where
  toCodeTree :: a -> CodeTree

instance ToCodeTree Lam.Module where
  toCodeTree (Lam.Module bindings) = CodeTree Nothing "Module" $ map toCodeTree bindings

instance ToCodeTree Lam.Binding where
  toCodeTree (Lam.Binding name exp) = CodeTree (Just name) "Binding" [toCodeTree exp]

instance ToCodeTree Lam.Exp where
  toCodeTree (Lam.Lit literal) = toCodeTree literal
  toCodeTree (Lam.Var name) = CodeTree (Just name) "Var" []
  toCodeTree (Lam.Lam args exp) = CodeTree Nothing "Lam" $ (map arg args) ++ [CodeTree Nothing "LamBody" [toCodeTree exp]]
    where arg name = CodeTree (Just name) "LamArg" []
  toCodeTree (Lam.App body args) = CodeTree Nothing "App" $ (CodeTree Nothing "AppBody" [toCodeTree body]):(map arg args)
    where arg exp = CodeTree Nothing "AppArg" [toCodeTree exp]

instance ToCodeTree Lam.Literal where
  toCodeTree (Lam.Number n) = CodeTree (Just $ T.pack $ show n) "LiteralNumber" []
  toCodeTree (Lam.Text t) = CodeTree (Just t) "LiteralText" []

newtype CodeDbId = CodeDbId Text
  deriving (Ord, Eq, Show)
codeDbIdText (CodeDbId id) = id

data CodeDbEntry = CodeDbEntry (Maybe Text) Text [CodeDbId]
data CodeDb = CodeDb { codeDbId :: CodeDbId, codeDbEntries :: (Map CodeDbId CodeDbEntry) }

type CodeDbIdGen a = ReaderT Text IO a

newtype SrcMapId = SrcMapId Integer
  deriving (Show,Ord,Eq)
newtype DstMapId = DstMapId Integer
  deriving (Show,Ord,Eq)
data SrcMapping = Delete | SrcChange DstMapId | SrcMove DstMapId
  deriving (Show)
data DstMapping = Add | DstChange SrcMapId | DstMove SrcMapId
  deriving (Show)

runCodeDbIdGen f = do
  baseUUID <- getBaseUUID
  runReaderT f (UUID.toText baseUUID)

getBaseUUID = do
  baseUUID <- Data.UUID.V1.nextUUID
  case baseUUID of
    Just u -> return u
    Nothing -> do
      Data.UUID.V4.nextRandom

nextCodeDbId :: CodeDbIdGen CodeDbId
nextCodeDbId = do
  baseUUID <- ask
  tailUUID <- liftIO Data.UUID.V4.nextRandom
  return $ CodeDbId $ baseUUID `T.append` UUID.toText tailUUID

main = do
  v1 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v1
  printdb v1
  v2 <- runCodeDbIdGen $ ingest $ toCodeTree Code.v2
  printdb v2
  diff <- diffCodeDbs v1 v2
  putStrLn $ T.pack $ show diff
  -- printDiff diff
  return ()

diffCodeDbs src dst = do
  let codeDbIds = codeDbIntKeys [src, dst]
  let q = encode $ object [
             "src" .= codeDbJson codeDbIds src,
             "dst" .= codeDbJson codeDbIds dst
          ]
  response <- post "http://localhost:4754/" q
  let mappingsJson = eitherDecode (response ^. responseBody) :: Either String Array
  let mappings = parseMappings =<< mappingsJson
  putStrLn $ T.pack $ show mappings

parseMappings :: Array -> Either String (Map SrcMapId SrcMapping, Map DstMapId DstMapping)
parseMappings = foldM parseMapping (Map.empty, Map.empty)
  where parseMapping (srcMappings, dstMappings) mapping = do
          flip parseEither mapping $ withObject "mapping was not an object" $ \obj -> do
            ty <- obj .: "ty"
            let getSrc = obj .: "src" >>= return . SrcMapId
            let getDst = obj .: "dst" >>= return . DstMapId
            case (ty :: Text) of
              "change" -> do
                src <- getSrc
                dst <- getDst
                return (Map.insert src (SrcChange dst) srcMappings
                       ,Map.insert dst (DstChange src) dstMappings)
              "move" -> do
                src <- getSrc
                dst <- getDst
                return (Map.insert src (SrcMove dst) srcMappings
                       ,Map.insert dst (DstMove src) dstMappings)
              "add" -> do
                dst <- getDst
                return (srcMappings
                       ,Map.insert dst Add dstMappings)
              "delete" -> do
                src <- getSrc
                return (Map.insert src Delete srcMappings
                       ,dstMappings)
  
  

codeDbIntKeys :: [CodeDb] -> Bimap CodeDbId Integer
codeDbIntKeys codeDbs = let allKeys = Map.keys (Map.unions $ map codeDbEntries codeDbs)
                            assocs = zip allKeys [0..] 
                         in Bimap.fromList assocs

codeDbJson codeDbIdInts codeDb@(CodeDb{..}) = codeDbJson' codeDbId
  where
    codeDbJson' codeDbId = object [
                             "id" .= fromJustNote ("codeDbJson' " ++ show codeDbId) (Bimap.lookup codeDbId codeDbIdInts),
                             "label" .= fromJustDef "" name,
                             "typeLabel" .= ty,
                             "children" .= map codeDbJson' children
                           ]
      where (CodeDbEntry name ty children) = dbEntry' codeDbId codeDbEntries

ingest :: CodeTree -> CodeDbIdGen CodeDb
ingest (CodeTree name ty children) = do
  id <- nextCodeDbId
  childDbs <- mapM ingest children
  let childIds = map codeDbId childDbs
  let childMaps = map codeDbEntries childDbs
  let childMap = Map.unions childMaps
  return $ CodeDb id (Map.insert id (CodeDbEntry name ty childIds) childMap)

printdb :: CodeDb -> IO ()
printdb = printdb' "" where
  printdb' prefix codeDb@(CodeDb {..}) = do
    let (CodeDbEntry name ty children) = dbEntry codeDb
    putStrLn $ prefix `T.append` ty `T.append` " " `T.append` fromJustDef "" name
    forM_ children (\childId -> printdb' (prefix `T.append` "  ") (CodeDb childId codeDbEntries))

dbEntry (CodeDb {..}) = dbEntry' codeDbId codeDbEntries
dbEntry' codeDbId codeDbEntries = fromJustDef (CodeDbEntry (Just $ codeDbIdText codeDbId) "MISSING" []) $ Map.lookup codeDbId codeDbEntries
