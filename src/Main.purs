module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..))

import Graphics.Canvas

import Partial.Unsafe (unsafePartial)

main :: forall e. Eff _ Unit
main = unsafePartial $ do
    Just canvas <- getCanvasElementById "game"
    
    pure unit