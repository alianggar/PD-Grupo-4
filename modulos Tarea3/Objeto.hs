module Objeto
  ( Objeto(..)
  , Plataforma(..), Meta(..), Enemigo(..)
  , solapan, colisionaCon, colisionaConAlguno
  ) where

import Tipos

data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)


class Objeto a where    
    posicion    :: a -> Vector2D
    boundingBox :: a -> BoundingBox
    animar      :: Float -> a -> a
    animar _ = id

data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
    deriving (Eq, Show)

instance Objeto Plataforma where
    posicion (Plataforma (x, y, _, _)) = (x, y)
    boundingBox (Plataforma bb) = bb

instance Objeto Meta where
    posicion m = posMeta m
    boundingBox (Meta (x, y)) = (x, y, 70, 80)

instance Objeto Enemigo where
    posicion e = posEnemigo e
    boundingBox e =
        let (x, y) = posEnemigo e
            (w, h) = dimEnemigo e
        in (x - w/2, y - h/2, w, h)
    animar dt e =
        let desp     = escalarVector dt (velEnemigo e)
            nuevaPos = sumaVectores (posEnemigo e) desp
        in e { posEnemigo = nuevaPos }

solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) =
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)

colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a xs = any (colisionaCon a) xs