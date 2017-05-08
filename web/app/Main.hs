module Main where

import Prelude hiding (id)

import qualified Unsafe.Coerce

import           Control.Monad (void)
import qualified Data.IORef as IORef
import qualified Data.JSString as JSS
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import qualified Data.Ratio as Rational
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(utctDay, utctDayTime), getCurrentTime)
import           Data.Time.Calendar (Day(toModifiedJulianDay))

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Attribute as A

import           GHCJS.Foreign.QQ
import           GHCJS.Types

import qualified Builtins
import qualified Code
import CodeDb (CodeDbId(..), codeDbIdText)
import qualified Eval
import Eval (Val(..))
import Lam (bindingsWithIdText, mapBindingId)
import Prim (Prim(..))
import qualified Run

textDiv :: String -> VNode
textDiv x = E.div () [ch|c|]
  where
    c = E.text . JSS.pack $ x


type Instant = [Integer]

data Event = Event { eventInstant :: Instant, eventDetails :: EventDetails }


data EventDetails = ClickEvent | ChangeEvent Text
  deriving (Eq, Show)

type AllEvents = Map (Eval.Frame CodeDbId) [Event]

data Property = TextValue
  deriving (Eq, Ord, Show)

type Universe = Map (Eval.Frame CodeDbId, Property) (Val CodeDbId)
type Universes = [(Instant, Universe)]

wrapEventDetails :: Event -> Eval.Val CodeDbId
wrapEventDetails e = case eventDetails e of
  ClickEvent -> Builtins.unionVal Map.empty (CodeDbId "builtin-htmlEventDetails-htmlEventDetails_click")
                                            (Primitive $ Prim.Text "")
  ChangeEvent t -> Builtins.unionVal Map.empty (CodeDbId "builtin-htmlEventDetails-htmlEventDetails_textChange")
                                               (Primitive $ Prim.Text t)

addEvent :: Event -> Eval.Frame CodeDbId -> AllEvents -> (AllEvents,AllEvents)
addEvent event frame allEvents = let result = addEvent' event frame allEvents
                                  in (result,result)

addEvent' :: Ord k => a -> k -> Map k [a] -> Map k [a]
addEvent' event = Map.alter (\events -> (event:) <$> (events `mappend` (Just []))) 

lastUniverse :: Universes -> Universe
lastUniverse [] = Map.empty
lastUniverse ((_, universe):_) = universe

expandUniverses :: Instant -> EventDetails -> Eval.Frame CodeDbId -> Universes -> (Universes,Universes)
expandUniverses instant eventDetails frame universes = let r = (instant, nextUniverse eventDetails (lastUniverse universes)):universes
                                                        in (r, r)
  where nextUniverse ClickEvent prev = prev
        nextUniverse (ChangeEvent text) prev = Map.insert (frame, TextValue) (Primitive $ Prim.Text text) prev

inputTextFromEvent :: Ev.Event -> IO (Maybe Text)
inputTextFromEvent ev = do let evVal = (Unsafe.Coerce.unsafeCoerce ev :: JSVal)
                           v <- [js| `evVal.target.value |]
                           pure v


renderVal :: (Show i) => (Eval.Frame i -> EventDetails -> IO ()) -> Val i -> VNode
renderVal _       (ValList [ Primitive (Text "text")
                           , Primitive (Text t)
                           ]) = textDiv . T.unpack $ t
renderVal onEvent (ValList [ Primitive (Text "button")
                           , Primitive (Text label)
                           , ValFrame frame
                           ]) = E.button (Ev.click (\_ -> onEvent frame ClickEvent))
                                         [E.text . JSS.pack . T.unpack $ label]
renderVal onEvent (ValList [ Primitive (Text "textInput")
                           , ValFrame frame
                           ]) = E.input (Ev.change (\e -> onEvent frame . ChangeEvent =<< (fromMaybe "" <$> inputTextFromEvent e)))
                                        ([] :: [VNode])

renderVal onEvent (ValList elems) = E.div () $ map (renderVal onEvent) elems
renderVal _ _                     = textDiv "That ain't no element"

envFromEvents :: Map (Eval.Frame CodeDbId) [Event] -> Map CodeDbId (Val CodeDbId)
envFromEvents = Map.fromList . map envFromEvent . Map.assocs
  where envFromEvent (frame, events) = (CodeDbId $ "events-" `T.append` (T.pack . show $ frame), ValList $ eventVal <$> events)
        eventVal event = Eval.ValFrame . Eval.Frame Nothing (CodeDbId "builtin-event") . Map.fromList $ 
                             [(CodeDbId "builtin-event-event_instant", Eval.ValList $ Eval.Primitive . Prim.Number <$> eventInstant event)
                             ,(CodeDbId "builtin-event-event_details", wrapEventDetails event)
                             ]

envFromUniverses :: Universes -> Map CodeDbId (Val CodeDbId)
envFromUniverses = Map.unions . fmap envFromUniverse 
  where envFromUniverse (instant, universe) = Map.fromList . map (envFromProp instant) . Map.assocs $ universe
        envFromProp instant ((frame, property), value) = (CodeDbId $ "props-" `T.append` (T.pack . show $ frame) `T.append` "-" `T.append` (T.pack . show $ instant) `T.append` "-" `T.append` (T.pack . show $ property), value)


newInstant :: UTCTime -> Set [Integer] -> (Set [Integer], [Integer])
newInstant time instants = (Set.insert free instants, free)
  where
    diffTimeRational = toRational $ utctDayTime time
    instantRoot = [ toModifiedJulianDay . utctDay $ time
                  , Rational.numerator diffTimeRational `div` Rational.denominator diffTimeRational
                  , Rational.numerator diffTimeRational `mod` Rational.denominator diffTimeRational
                  ]
    free = head . filter (\s -> not $ Set.member s instants) . map (\n -> instantRoot ++ [n]) $ [(0::Integer)..]


runWeb :: VMount -> IO ()
runWeb mountPoint = do (bindingsWithIds, _) <- Run.projectCode Code.web
                       putStrLn $ T.unpack $ bindingsWithIdText (mapBindingId codeDbIdText <$> bindingsWithIds)
                       events <- IORef.newIORef Map.empty
                       universes <- IORef.newIORef []
                       instants <- IORef.newIORef Set.empty
                       let rerender frame eventDetails = do id <- getCurrentTime >>= \time -> IORef.atomicModifyIORef instants (newInstant time)
                                                            currentEvents <- IORef.atomicModifyIORef events (addEvent (Event id eventDetails) frame)
                                                            currentUniverses <- IORef.atomicModifyIORef universes (expandUniverses id eventDetails frame)
                                                            render rerender mountPoint $ Run.runProjectionWithEnv bindingsWithIds (envFromEvents currentEvents `Map.union` envFromUniverses currentUniverses)
                       render rerender mountPoint $ Run.runProjectionWithEnv bindingsWithIds Map.empty

renderErrors :: [Run.Error] -> VNode
renderErrors = E.div ([] :: [A.Attribute]) . map renderError

renderError :: Run.Error -> VNode
renderError (Run.RuntimeError (Eval.TypeError i val message)) = E.div ([] :: [A.Attribute])
  [ E.h1 ([] :: [A.Attribute]) $ E.text . JSS.pack $ "Type Error at " ++ show i ++ T.unpack message
  , E.pre ([] :: [A.Attribute]) $ E.text . JSS.pack . Eval.pprintVal $ val
  ]
renderError (Run.RuntimeError (Eval.UndefinedMember i frame message)) = E.div ([] :: [A.Attribute])
  [ E.h1 ([] :: [A.Attribute]) $ E.text . JSS.pack $ "Undefined member at " ++ show i ++ T.unpack message
  , E.pre ([] :: [A.Attribute]) $ E.text . JSS.pack . Eval.pprintVal . Eval.ValFrame $ frame
  ]

renderError e = textDiv $ show e

render :: (Eval.Frame CodeDbId -> EventDetails -> IO ()) -> VMount -> Either [Run.Error] (Val CodeDbId) -> IO ()
render onEvent mountPoint val = do let vdom = either (renderErrors) (renderVal onEvent) val
                                   p <- diff mountPoint vdom
                                   void $ patch mountPoint p


main :: IO ()
main = do
  Ev.initEventDelegation Ev.defaultEvents
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  runWeb m
