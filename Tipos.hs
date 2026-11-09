module Tipos
  ( Vector2D, BoundingBox
  , sumaVectores, escalarVector
  , Celda(..), Grid
  , TipoEnemigo(..), EstadoJuego(..)
  , parsearCelda, parsearGrid, celdaAChar
  , esSolido, esMeta, esEnemigo
  ) where

-- Caja de colisión como (x, y, ancho, alto).
type BoundingBox = (Float, Float, Float, Float)

-- Vector o punto del plano como (x, y).
type Vector2D = (Float, Float)

-- Tipos de celda que puede haber en un mapa.
data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

-- Mapa del nivel como lista de filas de celdas.
type Grid = [[Celda]]

-- Tipos de enemigo según su comportamiento.
data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

-- Estados posibles de la partida.
data EstadoJuego
  = Jugando
  | PierdeVida
  | GameOver
  | NivelCompletado
  deriving (Eq, Show)

-- Multiplica un vector por un escalar.
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector z (x1, y1) = (x1 * z, y1 * z)

-- Suma dos vectores componente a componente.
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Convierte un carácter del mapa en su celda correspondiente.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte las líneas de texto de un mapa en una rejilla de celdas.
parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

-- Convierte una celda en su carácter de mapa (inversa de parsearCelda).
celdaAChar :: Celda -> Char
celdaAChar Solido                = '#'
celdaAChar Vacio                 = '.'
celdaAChar META                  = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo [])    = '.'

-- Indica si la celda es un bloque sólido.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

-- Indica si la celda es la meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

-- Indica si la celda contiene el inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False