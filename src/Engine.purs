module Engine where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Graphics.Canvas (CANVAS, Context2D)

foreign import data GAME :: Type -> Type -> Effect

type Game s e q = {
    init :: Eff ( game :: GAME s e | q ) s,
    update :: e -> s -> Eff ( game :: GAME s e | q ) s,
    draw :: Context2D -> s -> Eff ( canvas :: CANVAS, game :: GAME s e | q ) Unit
}

foreign import data GameManager :: Type -> Type -> Type

type GameEvent s e q = GameManager s e -> Eff ( game :: GAME s e | q ) Unit

foreign import runGame :: forall s e q. String -> Array (GameEvent s e q) -> Game s e q -> Eff ( game :: GAME s e ) (GameManager s e)

foreign import addGameEvent :: forall s e q. e -> GameManager s e -> Eff ( game :: GAME s e | q ) Unit
foreign import getState :: forall s e q. GameManager s e -> Eff ( game :: GAME s e | q ) s

foreign import requestAnimationFrame :: forall s e q. (Number -> Eff ( game :: GAME s e | q ) Unit) -> Eff ( game :: GAME s e | q ) Unit
foreign import onKeyDown :: forall s e q. (Int -> Eff ( game :: GAME s e | q ) Unit) -> Eff ( game :: GAME s e | q ) Unit