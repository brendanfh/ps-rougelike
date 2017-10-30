module Main2 where

import Control.Monad.Eff.Console
import Engine
import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Data.Int (toNumber)

data GameEvent = NoOp | Tick Number

main ::
  Eff                    
    ( game :: GAME Int GameEvent
    )
    (GameManager Int GameEvent)
main = do
    let game = {
            init : pure 0,
            update : \e s -> pure $ s + 1,
            draw : \ctx x -> do
                _ <- setFillStyle "black" ctx
                _ <- fillRect ctx { x : toNumber x, y: 0.0, w : 32.0, h : 32.0 }
                pure unit
    }

    let frame = \manager -> do
            let d = \dt -> do
                    addGameEvent (Tick dt) manager
                    requestAnimationFrame d
            requestAnimationFrame d

    runGame "game" [ frame ] game

