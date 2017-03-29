module Main where

import           Control.Monad (void)
import qualified Data.JSString as JSS

import           GHCJS.VDOM
import           GHCJS.VDOM.QQ
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A

import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.QQ
import           GHCJS.Types


textDiv :: Show a => a -> VNode
textDiv x = E.div () [ch|c|]
  where
    c = E.text . JSS.pack . show $ x

run :: VMount -> IO ()
run mountPoint = do let r = textDiv "WHAAAAT"
                    p <- diff mountPoint r
                    void $ patch mountPoint p

main :: IO ()
main = do
  root <- [js| document.createElement('div') |]
  [js_| document.body.appendChild(`root); |]
  m <- mount root (E.div () ())
  run m
