module Builtins (functionIds, argIds, allArgIds, EvalError(..), Builtin, env) where

import Control.Lens
import Control.Monad.Adaptive.Ref (Ref)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Eval (Env, Val(..), EvalError(..), Thunk(..))
import CodeDb (CodeDbId (..))

type Name = T.Text
type ArgName = T.Text
type Id = T.Text

data Builtin m r = Builtin { _name :: Name
                           , _argNames :: [ArgName]
                           , _body :: (CodeDbId -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId))
                           }
makeLenses ''Builtin

--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b

get :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] (Val m r CodeDbId)
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getInt :: CodeDbId -> T.Text -> Env m r CodeDbId -> Either [EvalError CodeDbId] Integer
getInt i argId env = get i argId env >>= \v ->
  case v of
    (Number num) -> pure num
    (Text text) -> Left [TypeError i $ text `T.append` " is text, not a number"]
    t@(Thunk _) -> Left [TypeError i $ (T.pack $ show t) `T.append` " is thunk, not a number"]

plus :: Builtin m r
plus = Builtin "+" ["+_1", "+_2"] $ \i e -> do
  n <- getInt i "builtin-+-+_1" e
  m <- getInt i "builtin-+-+_2" e
  pure $ Number $ n + m

builtins :: [Builtin m r]
builtins = [plus]

builtinId :: Builtin m r -> Id
builtinId builtin = "builtin-" `T.append` (builtin ^. name)

argId :: Builtin m r -> ArgName -> Id
argId builtin argName = builtinId builtin `T.append` "-" `T.append` argName


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]

argIds :: Builtin m r -> Set CodeDbId
argIds builtin = Set.fromList [CodeDbId $ argId builtin argName 
                              | argName <- builtin ^. argNames
                              ]

builtinArgsIds :: Builtin m r -> [(Name, CodeDbId)]
builtinArgsIds builtin = [(argName, CodeDbId $ argId builtin argName)| argName <- builtin ^. argNames]

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins

env :: Env IO IORef CodeDbId
env = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk . ThunkFn (CodeDbId $ builtinId builtin)
                           $ \env -> do (builtin ^. body) (CodeDbId (builtinId builtin)) env
          )
        | builtin <- builtins]
