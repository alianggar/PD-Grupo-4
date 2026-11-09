module Jugador
  ( Jugador
  , crearJugador, perderVida, sumarPuntos
  , conVelocidad, moverJugador, estadoSiguiente
  , posJugador, velJugador, vidas, puntuacion
  ) where

import Tipos
import Objeto

-- Jugador con posición, velocidad, vidas restantes y puntuación (constructor oculto).
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

-- Crea un jugador en la posición dada con velocidad nula, 3 vidas y 0 puntos.
crearJugador :: Vector2D -> Jugador
crearJugador posicionInicial = Jugador
  { posJugador = posicionInicial
  , velJugador = (0, 0)
  , vidas      = 3
  , puntuacion = 0
  }

-- Resta una vida al jugador sin bajar nunca de 0.
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Suma la cantidad indicada a la puntuación del jugador.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

-- Establece la velocidad del jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad nuevaVelocidad j =
  j { velJugador = nuevaVelocidad }

-- Desplaza al jugador su velocidad multiplicada por el tiempo transcurrido.
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j =
  j { posJugador =
        sumaVectores (posJugador j)
                     (escalarVector dt (velJugador j))
    }

-- Calcula el estado del juego a partir del jugador ya actualizado, el daño recibido y el contacto con la meta.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDano  = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando