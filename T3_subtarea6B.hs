import Graphics.Gloss

{-
### 6B. Del mundo a la pantalla (0.75 puntos)

Para probarlo en `ghci`, cargad también `import Graphics.Gloss`: un `Picture` de Gloss
se puede mostrar con `show` y comparar con `==`.

```haskell
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarHUD    :: Jugador -> Picture
mensajeEstado :: EstadoJuego -> Maybe String
dibujarMundo  :: Mundo -> Picture
```

| Función | Puntos | Criterio |
|---|---|---|
| `dibujarObjeto col o` | 0.25 | **función genérica sobre la clase `Objeto`**: un rectángulo sólido (`rectangleSolid`) de color `col` con el tamaño y la posición de su `boundingBox` (`translate` al **centro** de la caja) |
| `dibujarHUD` y `mensajeEstado` | 0.25 | `dibujarHUD`: el texto `"Vidas: v   Puntos: p"` (con tres espacios), con `translate (-380) 270` y `scale 0.15 0.15`. `mensajeEstado`: `Nothing` si se está `Jugando`; `"PIERDE VIDA (R para reiniciar)"`, `"GAME OVER"` y `"NIVEL COMPLETADO"` en `Just` para los otros estados |
| `dibujarMundo` | 0.25 | ver el recuadro de debajo |

`dibujarMundo` compone, con `Pictures`: la **escena** (todo lo del mundo, desplazada con
`translate (-px) (-py)` para que la cámara siga al jugador, siendo `(px, py)` su posición),
el HUD (**fijo**, sin desplazar) y, si `mensajeEstado` devuelve texto, el mensaje centrado
(`translate (-250) 0 (scale 0.3 0.3 (color black (text msg)))`). En la escena, por este orden
(el último se dibuja encima): plataformas en `greyN 0.5`, metas en `green`, enemigos en
`red`, y el jugador en `blue`. Cada grupo se dibuja con `map (dibujarObjeto ...)`.

**Ejemplos:**

```
> dibujarObjeto red (Meta (100, 40))
Translate 135.0 80.0 (Color (RGBA 1.0 0.0 0.0 1.0) (Polygon [(-35.0,-40.0),(-35.0,40.0),(35.0,40.0),(35.0,-40.0)]))
> dibujarObjeto red (Meta (100, 40)) == translate 135 80 (color red (rectangleSolid 70 80))
True
> dibujarHUD (sumarPuntos 50 (crearJugador (0, 0)))
Translate (-380.0) 270.0 (Scale 0.15 0.15 (Text "Vidas: 3   Puntos: 50"))
> map mensajeEstado [Jugando, PierdeVida, GameOver, NivelCompletado]
[Nothing,Just "PIERDE VIDA (R para reiniciar)",Just "GAME OVER",Just "NIVEL COMPLETADO"]
```
-}

-- ============================================================
-- TIPOS NECESARIOS
-- ============================================================

type Vector2D = (Float, Float)
type BoundingBox = (Float, Float, Float, Float)

data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

data EstadoJuego
  = Jugando
  | PierdeVida
  | GameOver
  | NivelCompletado
  deriving (Eq, Show)

data Celda
  = Solido
  | Vacio
  | META
  | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)


-- ============================================================
-- OPERACIONES CON VECTORES
-- ============================================================

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2,y1+y2) 

escalarVector :: Float -> Vector2D -> Vector2D
escalarVector z (x1,y1) = (x1*z,y1*z)

-- ============================================================
-- DE LA SUBTAREA 2
-- ============================================================

-- Crea un jugador parado, con tres vidas y cero puntos.
crearJugador :: (Float, Float) -> Jugador
crearJugador posicionInicial =
  Jugador
    { posJugador = posicionInicial
    , velJugador = (0, 0)
    , vidas      = 3
    , puntuacion = 0
    }

-- Añade puntos a la puntuación del jugador.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j =
  j { puntuacion = puntuacion j + n }

-- ============================================================
-- CLASE E INSTANCIAS DE LA SUBTAREA 3
-- ============================================================

class Objeto a where
  posicion    :: a -> Vector2D
  boundingBox :: a -> BoundingBox
  animar      :: Float -> a -> a
  animar _ = id

data Plataforma = Plataforma BoundingBox
  deriving (Eq, Show)

data Meta = Meta
  { posMeta :: Vector2D
  } deriving (Eq, Show)


instance Objeto Plataforma where
  posicion (Plataforma (x, y, _, _)) = (x, y)

  boundingBox (Plataforma bb) = bb


instance Objeto Meta where
  posicion m = posMeta m

  boundingBox (Meta (x, y)) = (x, y, 70, 80)


instance Objeto Jugador where
  posicion j = posJugador j

  boundingBox j =
    let (x, y) = posJugador j
    in (x - 30, y, 60, 90)


instance Objeto Enemigo where
  posicion e = posEnemigo e

  boundingBox e =
    let (x, y) = posEnemigo e
        (w, h) = dimEnemigo e
    in (x - w / 2, y - h / 2, w, h)

  animar dt e =
    let desp = escalarVector dt (velEnemigo e)
        nuevaPos = sumaVectores (posEnemigo e) desp
    in e { posEnemigo = nuevaPos }


-- ============================================================
-- SUBTAREA 6B
-- ============================================================

-- Dibuja el rectángulo de un objeto en el centro de su caja.
dibujarObjeto :: Objeto a => Color -> a -> Picture
dibujarObjeto col objeto =
  translate (x + ancho / 2) (y + alto / 2)
    (color col (rectangleSolid ancho alto))
  where
    (x, y, ancho, alto) = boundingBox objeto


-- Dibuja las vidas y los puntos en una posición fija de la pantalla.
dibujarHUD :: Jugador -> Picture
dibujarHUD j =
  translate (-380) 270
    (scale 0.15 0.15
      (text ("Vidas: " ++ show (vidas j)
          ++ "   Puntos: " ++ show (puntuacion j))))


-- Devuelve el mensaje correspondiente al estado del juego.
mensajeEstado :: EstadoJuego -> Maybe String
mensajeEstado Jugando =
  Nothing
mensajeEstado PierdeVida =
  Just "PIERDE VIDA (R para reiniciar)"
mensajeEstado GameOver =
  Just "GAME OVER"
mensajeEstado NivelCompletado =
  Just "NIVEL COMPLETADO"


-- Dibuja la escena con cámara, el HUD fijo y el mensaje de estado.
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