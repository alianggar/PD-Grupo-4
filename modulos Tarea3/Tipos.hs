module Tipos
  ( Vector2D, BoundingBox
  , sumaVectores, escalarVector
  , Celda(..), Grid
  , TipoEnemigo(..), EstadoJuego(..)
  , parsearCelda, parsearGrid, celdaAChar
  , esSolido, esMeta, esEnemigo
  ) where

type BoundingBox = (Float, Float, Float, Float)
type Vector2D=(Float,Float)

escalarVector :: Float -> Vector2D -> Vector2D
escalarVector z (x1,y1) = (x1*z,y1*z)

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2,y1+y2) 

data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

data EstadoJuego
  = Jugando
  | PierdeVida
  | GameOver
  | NivelCompletado
  deriving (Eq, Show)

parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo []) = '.'

esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False
