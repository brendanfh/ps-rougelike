module Main where

import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

getCanvas :: forall e. String -> Eff (canvas :: CANVAS | e) CanvasElement
getCanvas name = unsafePartial $ do
    Just canvas <- getCanvasElementById name
    pure canvas

main :: forall e. Eff _ Unit
main = do
    canvas <- getCanvas "game"
    ctx <- getContext2D canvas

    _ <- setFillStyle "black" ctx
    _ <- fillRect ctx { x : 0.0, y : 0.0, w : 1000.0, h : 1000.0 }
    pure unit
