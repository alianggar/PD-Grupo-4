module Mundo
  ( Mundo(..)
  , mundoInicial, actualizarMundo, manejarEvento
  ) where

import Tipos
import Objeto
import Jugador
import Nivel
import Graphics.Gloss.Interface.Pure.Game

data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

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

manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (100, vy) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (-100, vy) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 100) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, -100) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyUp) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }

manejarEvento (EventKey (SpecialKey KeyDown) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }

manejarEvento (EventKey (Char 'r') Down _ _) m =
    mundoInicial (nivelM m)

manejarEvento _ m = m