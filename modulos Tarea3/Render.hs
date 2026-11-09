module Render
  ( dibujarObjeto, dibujarHUD
  , mensajeEstado, dibujarMundo
  ) where

import Tipos
import Objeto
import Jugador
import Mundo
import Graphics.Gloss

-- Dibuja un rectángulo sólido de color col con el tamaño y la posición de la caja de colisión del objeto.
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col objeto =
  translate (x + ancho / 2) (y + alto / 2)
    (color col (rectangleSolid ancho alto))
  where
    (x, y, ancho, alto) = boundingBox objeto


-- Dibuja el texto fijo con las vidas y los puntos del jugador.
dibujarHUD :: Jugador -> Picture
dibujarHUD j =
  translate (-380) 270
    (scale 0.15 0.15
      (text ("Vidas: " ++ show (vidas j)
          ++ "   Puntos: " ++ show (puntuacion j))))


-- Devuelve el mensaje a mostrar según el estado del juego, o Nothing si se está jugando.
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando =
  Nothing
mensajeEstado PierdeVida =
  Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver =
  Just "GAME OVER"
mensajeEstado NivelCompletado =
  Just "NIVEL COMPLETADO"


-- Dibuja la escena con la cámara siguiendo al jugador, el HUD fijo y el mensaje de estado si lo hay.
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