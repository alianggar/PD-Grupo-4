module Nivel
  ( ErrorNivel(..), NivelData(..)
  , splitOn, buscarClave
  , parsearFichero, validarNivel, parsearNivelCompleto
  , tamCelda, posicionCelda
  , gridAPlataformas, metasDesdeGrid, enemigosDesdeGrid
  , parsearNiveles, todasLasPosiciones
  ) where

import Tipos
import Objeto
import qualified Data.List as L
import Text.Read (readMaybe)

-- Divide una cadena en trozos separados por el carácter dado.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, b)   -> a : splitOn c (tail b)

-- Devuelve lo que sigue a la clave en la primera línea que empieza por ella.
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case L.stripPrefix clave x of
  Just resto -> Just resto
  Nothing    -> buscarClave clave xs

-- Errores posibles al interpretar o validar un nivel.
data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

-- Datos de un nivel: nombre, fondos y mapa.
data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

-- Quita los espacios al principio y al final de una cadena.
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- Convierte un Maybe en un Either con un FormatoInvalido y el mensaje dado.
auxEither :: String -> Maybe a -> Either ErrorNivel a
auxEither msg Nothing  = Left (FormatoInvalido msg)
auxEither _   (Just x) = Right x

-- Interpreta las líneas de un fichero de nivel (cabecera, '---' y mapa) o devuelve el error de formato.
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
  let (cabecera, resto) = break (== "---") lineas
  if null resto
    then Left (FormatoInvalido "Falta el separador '---'")
    else do
      let mapaLineas = tail resto
      if null mapaLineas
        then Left (FormatoInvalido "Falta el mapa del nivel")
        else do
          nom    <- auxEither "Falta la clave NOMBRE:" (buscarClave "NOMBRE: " cabecera)
          fdsStr <- auxEither "Falta la clave FONDOS:" (buscarClave "FONDOS: " cabecera)
          let fdsText = splitOn ',' (trim fdsStr)
          fds    <- auxEither "Los fondos no son numéricos" (mapM readMaybe fdsText)
          let grid = parsearGrid mapaLineas
          return (NivelData (trim nom) fds grid)

-- Comprueba que el nivel tenga exactamente una meta.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd =
  let celdas   = concat (gridNivel nd)
      numMetas = length (filter esMeta celdas)
  in if numMetas == 0
       then Left NivelSinMeta
       else if numMetas > 1
              then Left (NivelConVariasMetas numMetas)
              else Right nd

-- Interpreta y valida un nivel encadenando parsearFichero y validarNivel.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas = parsearFichero lineas >>= validarNivel

-- Lado en unidades de cada celda del mapa.
tamCelda :: Float
tamCelda = 40

-- Calcula la esquina inferior izquierda de la celda (fila i desde arriba, columna j) en un mapa de total filas.
posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
  let x = fromIntegral j * tamCelda
      y = fromIntegral (total - 1 - i) * tamCelda
  in (x, y)

-- Crea una plataforma por cada celda sólida del mapa.
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
  let total = length grid
  in [ Plataforma (x, y, tamCelda, tamCelda)
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esSolido celda
     , let (x, y) = posicionCelda total i j
     ]

-- Crea una meta en la esquina de cada celda META del mapa.
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
  let total = length grid
  in [ Meta (posicionCelda total i j)
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esMeta celda
     ]

-- Crea un enemigo patrulla centrado en cada celda de inicio de enemigo del mapa.
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
  let total = length grid
  in [ Enemigo idEne (posicionCelda total i j) (-40, 0) (tamCelda, tamCelda) Patrulla
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , InicioEnemigo idEne <- [celda]
     ]

-- Interpreta y valida una lista de niveles, devolviendo todos o el primer error.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles niveles =
  traverse parsearNivelCompleto niveles

-- Lista todas las posiciones (fila, columna) de una rejilla rectangular, por filas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones g@(primeraFila : _) =
  (,) <$> [0 .. length g - 1]
      <*> [0 .. length primeraFila - 1]