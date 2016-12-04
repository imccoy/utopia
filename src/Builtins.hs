module Builtins (functionIds, allArgIds, EvalError(..), Builtin, env) where

import Control.Lens
import Data.IORef
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Eval (Env, Val(..), EvalError(..), Thunk(..))
import CodeDb (CodeDbId (..))
import qualified Prim

type Name = T.Text
type ArgName = T.Text
type Id = T.Text

data Builtin m r = Builtin { _name :: Name
                           , _argNames :: [ArgName]
                           , _body :: (CodeDbId -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId))
                           }
makeLenses ''Builtin

get :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId)
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getInt :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] Integer
getInt i argId env = get i argId env >>= \v ->
  case v of
    (Primitive (Prim.Number num)) -> pure num
    (Primitive p) -> Left [TypeError i $ T.pack $ show p ++ " is not a number"]
    t@(Thunk _ _ _) -> Left [TypeError i $ (T.pack $ show t) `T.append` " is thunk, not a number"]

-- this could be:
--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b
plus :: Builtin m r
plus = Builtin "+" ["+_1", "+_2"] $ \i e -> do
  n <- getInt i "builtin-+-+_1" e
  m <- getInt i "builtin-+-+_2" e
  pure $ Primitive $ Prim.Number $ n + m

builtins :: [Builtin m r]
builtins = [plus]

builtinId :: Builtin m r -> Id
builtinId builtin = "builtin-" `T.append` (builtin ^. name)


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]


builtinArgsIds :: Builtin m r -> [(Name, CodeDbId)]
builtinArgsIds builtin = [(argName, CodeDbId $ argId builtin argName)| argName <- builtin ^. argNames]
  where argId :: Builtin m r -> ArgName -> Id
        argId builtin argName = builtinId builtin `T.append` "-" `T.append` argName

builtinArgsIdsSet :: Builtin m r -> Set CodeDbId
builtinArgsIdsSet = Set.fromList . map snd . builtinArgsIds

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins

env :: Env IO IORef CodeDbId
env = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk (builtinArgsIdsSet builtin) Map.empty . ThunkFn (CodeDbId $ builtinId builtin)
                                                       $ \env -> do (builtin ^. body) (CodeDbId (builtinId builtin)) env
          )
        | builtin <- builtins]
