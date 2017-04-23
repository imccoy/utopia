module Main where

import Prelude hiding (id)

import Debug.Trace
import qualified Unsafe.Coerce

import           Control.Monad (void, filterM, join)
import qualified Data.IORef as IORef
import           Data.IORef (IORef)
import qualified Data.JSString as JSS
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import           Data.Text (Text)

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Event as Ev
import qualified GHCJS.VDOM.Attribute as A

import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.Types

import qualified Code
import CodeDb (CodeDbId(..))
import Prim (Prim(..))
import qualified Eval
import Eval (Val(..))
import qualified Run

textDiv :: String -> VNode
textDiv x = E.div () [ch|c|]
  where
    c = E.text . JSS.pack $ x

data Event = ClickEvent | ChangeEvent Text
  deriving (Eq, Show)

type AllEvents = Map (Eval.Frame CodeDbId) [Event]

addEvent :: Event -> Eval.Frame CodeDbId -> AllEvents -> (AllEvents,AllEvents)
addEvent event frame allEvents = let result = addEvent' event frame allEvents
                                  in (result,result)
addEvent' event = Map.alter (\events -> (event:) <$> (events `mappend` (Just []))) 

inputTextFromEvent :: Ev.Event -> IO (Maybe Text)
inputTextFromEvent ev = do let evVal = (Unsafe.Coerce.unsafeCoerce ev :: JSVal)
                           v <- [js| `evVal.target.value |]
                           pure v


renderVal :: (Show i) => (Eval.Frame i -> Event -> IO ()) -> Val i -> VNode
renderVal onEvent (ValList [ Primitive (Text "text")
                           , Primitive (Text t)
                           ]) = textDiv . T.unpack $ t
renderVal onEvent (ValList [ Primitive (Text "button")
                           , Primitive (Text label)
                           , ValFrame frame
                           ]) = E.button (Ev.click (\e -> onEvent frame ClickEvent))
                                         [E.text . JSS.pack . T.unpack $ label]
renderVal onEvent (ValList [ Primitive (Text "textInput")
                           , ValFrame frame
                           ]) = E.input (Ev.change (\e -> onEvent frame . ChangeEvent =<< (fromMaybe "" <$> inputTextFromEvent e)))
                                        ([] :: [VNode])

renderVal onEvent (ValList elems) = E.div () $ map (renderVal onEvent) elems

envFromEvents :: Map (Eval.Frame CodeDbId) [Event] -> Map CodeDbId (Val CodeDbId)
envFromEvents = Map.fromList . map envFromEvent . Map.assocs
  where envFromEvent (frame, events) = (CodeDbId $ "events-" `T.append` (T.pack . show $ frame), ValList $ eventVal <$> events)
        eventVal event = Eval.Primitive . Prim.Text . T.pack $ show event

runWeb :: VMount -> IO ()
runWeb mountPoint = do (bindingsWithIds, projection) <- Run.projectCode Code.web
                       events <- IORef.newIORef Map.empty 
                       let rerender frame event = do currentEvents <- IORef.atomicModifyIORef events (addEvent event frame)
                                                     render rerender mountPoint $ Run.runProjectionWithEnv bindingsWithIds (envFromEvents currentEvents)
                       render rerender mountPoint $ Run.runProjectionWithEnv bindingsWithIds Map.empty

render :: (Eval.Frame CodeDbId -> Event -> IO ()) -> VMount -> Either [Run.Error] (Val CodeDbId) -> IO ()
render onEvent mountPoint val = do let vdom = either (textDiv . show) (renderVal onEvent) val
                                   p <- diff mountPoint vdom
                                   void $ patch mountPoint p

main :: IO ()
main = do
  Ev.initEventDelegation Ev.defaultEvents
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  runWeb m
