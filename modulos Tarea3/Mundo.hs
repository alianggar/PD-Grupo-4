module Mundo
  ( Mundo(..)
  , mundoInicial, actualizarMundo, manejarEvento
  ) where

import Tipos
import Objeto
import Jugador
import Nivel
import Graphics.Gloss.Interface.Pure.Game

-- Estado completo del juego: jugador, enemigos, plataformas, metas, estado y nivel.
data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

-- Construye el mundo de partida a partir de los datos de un nivel.
mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM     = crearJugador (60, 120)
  , enemigosM    = enemigosDesdeGrid g
  , plataformasM = gridAPlataformas g
  , metasM       = metasDesdeGrid g
  , estadoM      = Jugando
  , nivelM       = nd
  }
  where g = gridNivel nd

-- Avanza la simulación dt segundos: mueve jugador y enemigos, detecta daño y meta y actualiza el estado.
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
    | estadoM m /= Jugando = m
    | otherwise =
        let jug              = jugadorM m
            enemigos         = enemigosM m
            metas            = metasM m
            jugMovido        = moverJugador dt jug
            enemigosAnimados = map (animar dt) enemigos
            tocaEnemigo      = colisionaConAlguno jugMovido enemigosAnimados
            tocaMeta         = colisionaConAlguno jugMovido metas
            jugFinal         = if tocaEnemigo then perderVida jugMovido else jugMovido
            nuevoEstado      = estadoSiguiente jugFinal tocaEnemigo tocaMeta
        in m { jugadorM  = jugFinal
             , enemigosM = enemigosAnimados
             , estadoM   = nuevoEstado
             }

-- Traduce las flechas del teclado en velocidad del jugador y la tecla 'r' en reinicio del nivel.
manejarEvento :: Event -> Mundo -> Mundo
-- Flecha derecha pulsada: velocidad horizontal 100.
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (100, vy) (jugadorM m) }

-- Flecha izquierda pulsada: velocidad horizontal -100.
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (-100, vy) (jugadorM m) }

-- Flecha derecha soltada: anula la velocidad horizontal.
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }

-- Flecha izquierda soltada: anula la velocidad horizontal.
manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }

-- Flecha arriba pulsada: velocidad vertical 100.
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 100) (jugadorM m) }

-- Flecha abajo pulsada: velocidad vertical -100.
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, -100) (jugadorM m) }

-- Flecha arriba soltada: anula la velocidad vertical.
manejarEvento (EventKey (SpecialKey KeyUp) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }

-- Flecha abajo soltada: anula la velocidad vertical.
manejarEvento (EventKey (SpecialKey KeyDown) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }

-- Tecla 'r' pulsada: reinicia el nivel actual.
manejarEvento (EventKey (Char 'r') Down _ _) m =
    mundoInicial (nivelM m)

-- Cualquier otro evento: el mundo no cambia.
manejarEvento _ m = m