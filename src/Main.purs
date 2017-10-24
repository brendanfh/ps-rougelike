module Main where

import Graphics.Canvas
import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref 
import Data.Int (toNumber)
import Data.List
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (for)
import Partial.Unsafe (unsafePartial)

import Player as P
import Util

foreign import requestAnimationFrame :: (Number -> GameEff Unit) -> GameEff Unit
foreign import onKeyDown :: (Int -> GameEff Unit) -> GameEff Unit

data GameEvent = NoOp | Redraw | KeyDown Int | KeyUp Int

type Game = {
    player :: P.Player,
    eventQueue :: List GameEvent
}

initial :: GameEff (Ref Game)
initial = do
    let game =
            { player : P.create 0.0 0.0
            , eventQueue : Nil
            }
    gameRef <- newRef game
    pure gameRef

addGameEvent :: GameEvent -> Game -> Game
addGameEvent ge game = game { eventQueue = Cons ge game.eventQueue }

getCanvas :: String -> GameEff CanvasElement
getCanvas name = unsafePartial $ do
    Just canvas <- getCanvasElementById name
    pure canvas

drawLitGrid :: Int -> Int -> Number -> Point -> Context2D -> GameEff Unit
drawLitGrid w h size light ctx = do
    let nw = toNumber w
    let nh = toNumber h

    forE 0 h \y -> do
        let ny = toNumber y
        forE 0 w \x -> do
            let nx = toNumber x
            let br = (brightness nx ny light) * 0.9 + 0.1
            let br1 = "rgba(255, 255, 255, " <> (show br) <> ")"
            let br2 = "rgba(100, 100, 100, " <> (show br) <> ")"

            _ <- setFillStyle br1 ctx
            _ <- fillRect ctx { x : nx * size, y : ny * size, w : size, h : size }
            _ <- setFillStyle br2 ctx
            _ <- fillRect ctx { x : nx * size + 4.0, y : ny * size + 4.0, w : size - 4.0, h : size - 4.0 }
            pure unit
    where
        brightness :: Number -> Number -> Point -> Number
        brightness x y (Point light) =
            let
                dist = abs (x - light.x) + abs (y - light.y)
            in
                max 0.0 ((5.0 - dist) / 5.0)

drawGame :: Context2D -> Game -> GameEff Unit
drawGame ctx { player } = do
    _ <- setFillStyle "black" ctx
    _ <- fillRect ctx { x : 0.0, y : 0.0, w : 1000.0, h : 1000.0 }

    drawLitGrid 20 15 32.0 player.pos ctx
    P.draw ctx player

--Returns true if a redraw is requested
processEvents :: Ref Game -> GameEff Boolean
processEvents gameRef = do
    game <- readRef gameRef

    redraws <- for game.eventQueue (processEvent gameRef)
    let redraw = foldl (||) false redraws

    modifyRef gameRef (_ { eventQueue = Nil })
    pure redraw

    where
        processEvent :: Ref Game -> GameEvent -> GameEff Boolean
        processEvent gameRef (KeyDown code) = do
            let dx = case code of
                    87 -> Point { x : 0.0, y : -1.0 }
                    83 -> Point { x : 0.0, y : 1.0 }
                    65 -> Point { x : -1.0, y : 0.0 }
                    68 -> Point { x : 1.0, y : 0.0 }
                    _ -> zero

            modifyRef gameRef $ \g -> g { player = P.move dx g.player}
            pure true
        
        processEvent _ Redraw = pure true
        processEvent _ _ = pure false

main :: GameEff Unit
main = do
    canvas <- getCanvas "game"
    _ <- setCanvasDimensions { width : 640.0, height : 480.0 } canvas
    ctx <- getContext2D canvas

    gameRef <- initial

    onKeyDown (\key -> modifyRef gameRef $ addGameEvent $ KeyDown key)

    modifyRef gameRef $ addGameEvent $ Redraw

    let mainLoop dt = do
            redraw <- processEvents gameRef

            ifM (pure redraw) (do
                game <- readRef gameRef
                drawGame ctx game
            ) (pure unit)

            requestAnimationFrame mainLoop
    requestAnimationFrame mainLoop

    pure unit