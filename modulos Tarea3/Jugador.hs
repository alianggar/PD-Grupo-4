module Jugador
  ( Jugador
  , crearJugador, perderVida, sumarPuntos
  , conVelocidad, moverJugador, estadoSiguiente
  , posJugador, velJugador, vidas, puntuacion 
  ) where

import Tipos
import Objeto


data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

instance Objeto Jugador where
    posicion j = posJugador j
    boundingBox j =
        let (x, y) = posJugador j
        in (x - 30, y, 60, 90)

crearJugador :: Vector2D -> Jugador
crearJugador posicionInicial = Jugador
  { posJugador = posicionInicial
  , velJugador = (0, 0)
  , vidas      = 3
  , puntuacion = 0
  }

perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }


sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad nuevaVelocidad j =
  j { velJugador = nuevaVelocidad }

moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j =
  j { posJugador =
        sumaVectores (posJugador j)
                     (escalarVector dt (velJugador j))
    }


estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDano  = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando
