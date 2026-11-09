module Render
  ( dibujarObjeto, dibujarHUD
  , mensajeEstado, dibujarMundo
  ) where

import Tipos
import Objeto
import Jugador
import Mundo
import Graphics.Gloss

dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col objeto =
  translate (x + ancho / 2) (y + alto / 2)
    (color col (rectangleSolid ancho alto))
  where
    (x, y, ancho, alto) = boundingBox objeto


dibujarHUD :: Jugador -> Picture
dibujarHUD j =
  translate (-380) 270
    (scale 0.15 0.15
      (text ("Vidas: " ++ show (vidas j)
          ++ "   Puntos: " ++ show (puntuacion j))))


mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando =
  Nothing
mensajeEstado PierdeVida =
  Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver =
  Just "GAME OVER"
mensajeEstado NivelCompletado =
  Just "NIVEL COMPLETADO"


dibujarMundo :: Mundo -> Picture
dibujarMundo m =
  Pictures
    [ translate (-px) (-py) escena
    , dibujarHUD (jugadorM m)
    , mensaje
    ]
  where
    (px, py) = posJugador (jugadorM m)

    escena =
      Pictures
        (map (dibujarObjeto (greyN 0.5)) (plataformasM m)
        ++ map (dibujarObjeto green) (metasM m)
        ++ map (dibujarObjeto red) (enemigosM m)
        ++ [dibujarObjeto blue (jugadorM m)])

    mensaje =
      case mensajeEstado (estadoM m) of
        Nothing -> Blank
        Just msg ->
          translate (-250) 0
            (scale 0.3 0.3
              (color black (text msg)))