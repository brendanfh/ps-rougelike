module Player where

import Graphics.Canvas
import Prelude
import Util

data Direction = North | South | East | West

type Player =
    { pos :: Point
    , direction :: Direction
    }

create :: Number -> Number -> Player
create x y =
    { pos : Point { x : x, y : y }
    , direction : North
    }

move :: Point -> Player -> Player
move d player@{ pos } =
    player { pos = pos + d }


draw :: Context2D -> Player -> GameEff Unit
draw ctx { pos } = do
    _ <- setFillStyle "red" ctx

    let (Point p) = pos
    _ <- fillRect ctx { x : p.x * 32.0, y : p.y * 32.0, w : 32.0, h : 32.0 }

    pure unit
