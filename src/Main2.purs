module Main2 where

import Control.Monad.Eff.Console
import Engine
import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff)
import Data.Int (toNumber)

data GameEvent = NoOp | Tick Number | KeyDown Int

init = pure 0

update :: GameEvent -> Int -> Eff _ Int
update e s = do
    pure $ s + 1
    
draw :: Context2D -> Int -> Eff _ Unit
draw ctx s = do
    _ <- setFillStyle "black" ctx
    _ <- fillRect ctx { x : toNumber s, y: 0.0, w : 32.0, h : 32.0 }
    pure unit

main ::
  Eff                    
    ( game :: GAME Int GameEvent
    )
    (GameManager Int GameEvent)
main = do
    let game = {
            init : init,
            update : update,
            draw : draw
    }

    let frame = \manager -> do
            let d = \dt -> do
                    addGameEvent (Tick dt) manager
                    requestAnimationFrame d
            requestAnimationFrame d
    
    let keydown = \manager -> do
            onKeyDown (\key-> addGameEvent (KeyDown key) manager)

    runGame "game" [ keydown ] game

