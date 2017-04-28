module Builtins (functionIds, allArgIds, EvalError(..), Builtin, builtinsEnv, unionVal) where

import Control.Lens
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T


import qualified Id
import Eval (Env, Val(..), EvalError(..), Thunk(..), Trail, Trailing, noTrail, Suspension(..), Frame, frameCodeDbId, eitherList)
import qualified Eval as Eval
import CodeDb (CodeDbId (..), codeDbIdText)
import qualified Lam
import qualified Prim

type Name = T.Text
type ArgName = T.Text
type Id = T.Text

data Builtin = Builtin { _name :: Name
                       , _argNames :: [ArgName]
                       , _body :: BuiltinBody
                       }

type BuiltinEnv = Env CodeDbId
type BuiltinGlobalEnv = Eval.GlobalEnv CodeDbId
type BuiltinVal = Val CodeDbId
type BuiltinThunk = Id.WithId CodeDbId Identity (Thunk CodeDbId)
type BuiltinSuspension = Suspension CodeDbId
type BuiltinTrail = Trail CodeDbId
type BuiltinFrame = Frame CodeDbId
type BuiltinTrailing = Trailing CodeDbId

data BuiltinBody = FnBody (CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | ResolvedFnBody (BuiltinFrame -> CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Lam.Resolved CodeDbId -> Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))
                 | TrailFnBody (CodeDbId -> Trail CodeDbId -> BuiltinEnv -> BuiltinGlobalEnv -> Either [EvalError CodeDbId] BuiltinVal)
                 | EvalFnBody ( CodeDbId
                              -> BuiltinEnv
                              -> BuiltinGlobalEnv
                              -> (BuiltinVal -> 
                                  Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))
                              -> Either [EvalError CodeDbId] (BuiltinTrailing BuiltinVal))
                 | Record

makeLenses ''Builtin

get :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinVal
get i argId env = case Map.lookup (CodeDbId argId) env of
                    Just v -> Right v
                    Nothing -> Left [UndefinedVar i argId]

getList :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] [BuiltinVal]
getList i argId env = get i argId env >>= asList i

asList :: i -> Val i -> Either [EvalError i] [Val i]
asList i v = case v of
  (ValList list) -> pure list 
  _ -> Left [TypeError i v $ " is not a list"]

getSuspension :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] BuiltinSuspension
getSuspension i argId env = get i argId env >>= \v ->
  case v of
    (ValSuspension suspension) -> Right suspension
    _ -> Left [TypeError i v " is not a suspension"]

getPrim :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Prim.Prim
getPrim i argId env = get i argId env >>= asPrim i

getFrame :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] (Frame CodeDbId)
getFrame i argId env = get i argId env >>= \v ->
  case v of
    ValFrame frame -> Right frame
    _ -> Left [TypeError i  v "is not a frame"]

asPrim :: i -> Val i -> Either [EvalError i] Prim.Prim
asPrim i v = case v of
  Primitive p -> Right p
  _ -> Left [TypeError i v "is not a prim"]

asNumber :: i -> Prim.Prim -> Either [EvalError i] Integer
asNumber i v = case v of
                 (Prim.Number num) -> pure num
                 p -> Left [TypeError i (Primitive p) " is not a number"]

getNumber :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Integer
getNumber i argId env = getPrim i argId env >>= asNumber i

getText :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] T.Text
getText i argId env = getPrim i argId env >>= \v ->
  case v of
    (Prim.Text text) -> pure text
    p -> Left [TypeError i (Primitive p) " is not text"]




type Thunkable = (Set CodeDbId, BuiltinEnv, BuiltinThunk)
getThunkable :: CodeDbId -> T.Text -> BuiltinEnv -> Either [EvalError CodeDbId] Thunkable
getThunkable i argId env = get i argId env >>= \v ->
  case v of
    Thunk needed thunkEnv thunkBody -> Right (needed, thunkEnv, thunkBody)
    _ -> Left [TypeError i v " is not a thunk"]


numberToText :: Builtin
numberToText = Builtin "numberToText" ["numberToText_number"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-numberToText-numberToText_number" e
  pure $ Primitive $ Prim.Text $ T.pack $ show n


-- this could be something like:
--plus = builtin "+" $ do
--         arg1 <- arg "1"
--         arg2 <- arg "2"
--         pure $ 
--           a <- askInt arg1
--           b <- askInt arg2
--           pure $ a + b
plus :: Builtin
plus = Builtin "+" ["+_1", "+_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin-+-+_1" e
  m <- getNumber i "builtin-+-+_2" e
  pure $ Primitive $ Prim.Number $ n + m

minus :: Builtin
minus = Builtin "-" ["-_1", "-_2"] . FnBody $ \i e _ -> do
  n <- getNumber i "builtin----_1" e
  m <- getNumber i "builtin----_2" e
  pure $ Primitive $ Prim.Number $ n - m


listAppend :: Builtin
listAppend = Builtin "listAppend" ["listAppend_1", "listAppend_2"] . FnBody $ \i e _ -> do
  n <- getList i "builtin-listAppend-listAppend_1" e
  m <- getList i "builtin-listAppend-listAppend_2" e
  pure $ ValList $ n ++ m

listConcat :: Builtin
listConcat = Builtin "listConcat" ["listConcat_lists"] . FnBody $ \i e _ -> do
  listVals <- getList i "builtin-listConcat-listConcat_lists" e
  lists <- eitherList $ asList i <$> listVals
  pure $ ValList $ concat lists



listAdd :: Builtin
listAdd = Builtin "listAdd" ["listAdd_elem", "listAdd_list"] . FnBody $ \i e _ -> do
  n <- get i "builtin-listAdd-listAdd_elem" e
  m <- getList i "builtin-listAdd-listAdd_list" e
  pure $ ValList $ n:m

listMap :: Builtin
listMap = Builtin "listMap" ["listMap_f", "listMap_list"] . EvalFnBody $ \i e _ eval -> do
  thunks <- do thunkable <- getThunkable i "builtin-listMap-listMap_f" e
               list <- getList i "builtin-listMap-listMap_list" e
               eitherList $ callOneArgThunkable i "listMap_f" thunkable <$> list
  vals <- mapM eval thunks
  pure $ ValList <$> sequenceA vals

callOneArgThunkable :: Ord i => i -> T.Text -> (Set i, Map i (Val i), Eval.ThunkWithId i) -> Val i -> Either [EvalError i] (Val i)
callOneArgThunkable i label (thunkNeeded, thunkEnv, thunkBody) argValue = case Set.toList thunkNeeded of
  [argId] -> Right $ Thunk Set.empty (Map.insert argId argValue thunkEnv) thunkBody
  [] -> Left [TypeError i (Thunk thunkNeeded thunkEnv thunkBody) $ "No arg for " `T.append` label]
  _ -> Left [TypeError i (Thunk thunkNeeded thunkEnv thunkBody) $ "Too many args for " `T.append` label]

listFilter :: Builtin
listFilter = Builtin "listFilter" ["listFilter_f", "listFilter_list"] . EvalFnBody $ \i e _ eval -> do
  list <- getList i "builtin-listFilter-listFilter_list" e
  thunks <- do (thunkNeeded, thunkEnv, thunkBody) <- getThunkable i "builtin-listFilter-listFilter_f" e
               case Set.toList thunkNeeded of
                 [argId] -> pure [ Thunk Set.empty (Map.insert argId listElem thunkEnv) thunkBody
                                 | listElem <- list]
                 [] -> Left [TypeError i (Thunk thunkNeeded thunkEnv thunkBody) "No arg for listFilter_f"]
                 _ -> Left [TypeError i (Thunk thunkNeeded thunkEnv thunkBody) "Too many args for listFilter_f"]
  ts <- sequenceA <$> mapM eval thunks
  let matchListVs vs = zipWith (\l v -> if v == Primitive (Prim.Number 0) then Nothing else Just l) list vs
  let trail_filtered = (\vs -> catMaybes . matchListVs $ vs) <$> ts
  pure $ ValList <$> trail_filtered


listEmpty :: Builtin
listEmpty = Builtin "listEmpty" [] . FnBody $ \_ _ _ -> do
  pure $ ValList []

listLength :: Builtin
listLength = Builtin "listLength" ["listLength_list"] . FnBody $ \i e _ -> do
  list <- getList i "builtin-listLength-listLength_list" e
  pure $ Primitive $ Prim.Number $ fromIntegral $ length list

listSum :: Builtin
listSum = Builtin "listSum" ["listSum_list"] . FnBody $ \i e _ -> do
  list <- getList i "builtin-listSum-listSum_list" e
  case partitionEithers ((asPrim i >=> asNumber i) <$> list) of
    ([], ints) -> pure $ Primitive $ Prim.Number $ fromIntegral $ sum ints
    (errs, _) -> Left $ concat errs


frameArg :: Builtin
frameArg = Builtin "frameArg" ["frameArg_frame", "frameArg_arg"] . ResolvedFnBody $ \_ i e _ _ -> do
  frame <- getFrame i "builtin-frameArg-frameArg_frame" e
  (Suspension argId _ _) <- getSuspension i "builtin-frameArg-frameArg_arg" e
  maybe (Left [UndefinedVar i (codeDbIdText argId)]) (Right . noTrail) $ Map.lookup argId (frame ^. Eval.frameEnv)

htmlElementEvents :: Builtin
htmlElementEvents = Builtin "htmlElementEvents" ["htmlElementEvents_element"] . ResolvedFnBody $ \_ i e g _ -> do
  let eventsByToken frame = case Eval.lookupVarByResolvedId e g (CodeDbId $ "events-" `T.append` (T.pack . show $ frame)) of
                              Just (ValList v) -> Right . noTrail $ ValList v
                              Nothing -> Right . noTrail . ValList $ []
                              Just v -> Left [TypeError i v $ "non-events at " `T.append` (T.pack . show $ frame)]
  getList i "builtin-htmlElementEvents-htmlElementEvents_element" e >>= \case
    [Primitive (Prim.Text "button"), _, ValFrame frame] -> eventsByToken frame
    [Primitive (Prim.Text "textInput"), ValFrame frame] -> eventsByToken frame
    fields -> Left [TypeError i (ValList fields) $ " is not an element with events"]


htmlText :: Builtin
htmlText = Builtin "htmlText" ["htmlText_text"] . FnBody $ \i e _ -> do
  text <- getText i "builtin-htmlText-htmlText_text" e
  pure $ ValList [Primitive $ Prim.Text "text", Primitive $ Prim.Text text]

htmlButton :: Builtin
htmlButton = Builtin "htmlButton" ["htmlButton_text"] . ResolvedFnBody $ \frame i e _ _ -> do
  text <- getText i "builtin-htmlButton-htmlButton_text" e
  pure $ noTrail $ ValList [Primitive $ Prim.Text "button", Primitive $ Prim.Text text, ValFrame frame]

htmlTextInput :: Builtin
htmlTextInput = Builtin "htmlTextInput" [] . ResolvedFnBody $ \frame _ _ _ _ -> do
  pure $ noTrail $ ValList [Primitive $ Prim.Text "textInput", ValFrame frame]


suspensionFrameList :: Builtin
suspensionFrameList = Builtin "suspensionFrameList" ["suspensionFrameList_suspension"] . TrailFnBody $ \i trail e _ -> do
  suspension <- getSuspension i "builtin-suspensionFrameList-suspensionFrameList_suspension" e
  pure $ ValList [ ValFrame frame
                 | frame <- suspensionFrames trail suspension]

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f = catMaybes . takeWhile isJust . iterate (>>= f) . Just

iterateUntilNothing' :: (Show a) => (a -> Maybe a) -> Maybe a -> [a]
iterateUntilNothing' f = fromMaybe [] . fmap (iterateUntilNothing f)


suspensionFrames :: BuiltinTrail -> BuiltinSuspension -> [BuiltinFrame]
suspensionFrames trail (Suspension suspensionResolvedId suspensionEnv suspensionParents) = 
  [ frame
  | (frame, _) <- Set.toList trail
  , frame ^. frameCodeDbId == suspensionResolvedId
  , Map.isSubmapOf suspensionEnv (frame ^. Eval.frameEnv)
  , suspensionParentsMatchFrameAncestors (frame ^. Eval.frameParent) suspensionParents
  ]
  where suspensionParentsMatchFrameAncestors parentFrame suspensionParents' = all (suspensionMatchesAFrame (iterateUntilNothing' (^. Eval.frameParent) parentFrame)) suspensionParents'

        suspensionMatchesAFrame parentFrames suspension = not . null $ List.intersect (suspensionFrames trail suspension) parentFrames


frameResult :: Builtin
frameResult = Builtin "frameResult" ["frameResult_frame"] . TrailFnBody $ \i trail e _ -> do
  targetFrame <- getFrame i "builtin-frameResult-frameResult_frame" e
  let results = [ result
                | (frame, result) <- Set.toList trail
                , frame == targetFrame
                ]
  case results of
    [] -> Left [UndefinedVar i "NO RESULT"]
    xs -> Right $ last xs

event :: Builtin
event = Builtin "event" ["event_instant", "event_details"] Record

construct :: Builtin
construct = Builtin "construct" ["construct_with", "construct_payload"] $ FnBody $ \i e _ -> do
  (Suspension constructorId _ _) <- getSuspension i "builtin-construct-construct_with" e
  payload <- get i "builtin-construct-construct_payload" e
  Right $ unionVal e constructorId payload


unionVal :: BuiltinEnv -> CodeDbId -> BuiltinVal -> BuiltinVal
unionVal env constructorId payload = Thunk (Set.fromList [constructorId]) env $ Id.WithId dubiousId $ Identity $ ThunkFn $ \e _ -> do
    caseThunkable <- getThunkable dubiousId (codeDbIdText constructorId) e
    callOneArgThunkable dubiousId  "construct" caseThunkable payload
  where dubiousId = CodeDbId "builtin-construct-unionVal"

builtins :: [Builtin]
builtins = [ event, construct
           , plus, minus
           , listAppend, listAdd, listConcat, listEmpty, listMap, listFilter, listLength, listSum
           , numberToText
           , suspensionFrameList, frameArg, frameResult
           , htmlText, htmlButton, htmlTextInput, htmlElementEvents
           ]

data BuiltinUnion = BuiltinUnion Name [Name]

htmlEventDetails :: BuiltinUnion
htmlEventDetails = BuiltinUnion "htmlEventDetails" ["htmlEventDetails_textChange", "htmlEventDetails_click"]

builtinUnions :: [BuiltinUnion]
builtinUnions = [ htmlEventDetails ]

builtinId :: Builtin -> Id
builtinId builtin = builtinIdFromName (builtin ^. name)

builtinIdFromName :: T.Text -> Id
builtinIdFromName builtinName = "builtin-" `T.append` builtinName


functionIds :: Map Name CodeDbId
functionIds = Map.fromList [(builtin ^. name, CodeDbId $ builtinId builtin) | builtin <- builtins]

argsIdsFromPrefix :: T.Text -> [T.Text] -> [(Name, CodeDbId)]
argsIdsFromPrefix prefix names = [(argName, CodeDbId $ argId argName)| argName <- names]
  where argId :: ArgName -> Id
        argId argName = prefix `T.append` "-" `T.append` argName



builtinArgsIds :: Builtin -> [(Name, CodeDbId)]
builtinArgsIds builtin = argsIdsFromPrefix (builtinId builtin) (builtin ^. argNames)

builtinArgsIdsSet :: Builtin -> Set CodeDbId
builtinArgsIdsSet = Set.fromList . map snd . builtinArgsIds

builtinUnionsArgsIds :: BuiltinUnion -> [(Name, CodeDbId)]
builtinUnionsArgsIds (BuiltinUnion unionName constructors) = argsIdsFromPrefix (builtinIdFromName unionName) constructors

allArgIds :: Map Name CodeDbId
allArgIds = Map.fromList $ foldMap builtinArgsIds builtins ++ foldMap builtinUnionsArgsIds builtinUnions

builtinsEnv :: Env CodeDbId
builtinsEnv = Map.fromList 
        [ (CodeDbId $ builtinId builtin
          , Thunk (builtinArgsIdsSet builtin) Map.empty . Id.withId (CodeDbId $ builtinId builtin) $ case builtin ^. body of
                                                            FnBody fn -> ThunkFn $ \env globalEnv -> do fn (CodeDbId $ builtinId builtin) env globalEnv
                                                            ResolvedFnBody fn -> ThunkResolvedFn $ \magic env globalEnv resolved -> do fn magic (CodeDbId $ builtinId builtin) env globalEnv resolved
                                                            EvalFnBody fn -> ThunkEvalFn $ \env globalEnv eval -> fn (CodeDbId $ builtinId builtin) env globalEnv eval
                                                            TrailFnBody fn -> ThunkTrailFn $ \trail env globalEnv -> do fn (CodeDbId $ builtinId builtin) trail env globalEnv
                                                            Record -> ThunkRecord
          )
        | builtin <- builtins]
