module Util where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Graphics.Canvas (CANVAS)

type GameEff r = Eff ( canvas :: CANVAS, console :: CONSOLE, ref :: REF ) r

newtype Point = Point { x :: Number, y :: Number }

instance semiringPoint :: Semiring Point where
    add (Point p1) (Point p2) = Point { x : p1.x + p2.x, y : p1.y + p2.y }
    zero = Point { x : 0.0, y : 0.0 }
    mul (Point p1) (Point p2) = Point { x : p1.x * p2.x, y : p1.y * p2.y }
    one = Point { x : 1.0, y : 1.0 }