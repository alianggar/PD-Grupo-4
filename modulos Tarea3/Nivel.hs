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

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, b)   -> a : splitOn c (tail b)

buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case L.stripPrefix clave x of
  Just resto -> Just resto
  Nothing    -> buscarClave clave xs


data ErrorNivel
  = FormatoInvalido String
  | NivelSinMeta
  | NivelConVariasMetas Int
  deriving (Eq, Show)

data NivelData = NivelData
  { nombreNivel :: String
  , fondos      :: [Int]
  , gridNivel   :: Grid
  } deriving (Eq, Show)

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

auxEither :: String -> Maybe a -> Either ErrorNivel a
auxEither msg Nothing  = Left (FormatoInvalido msg)
auxEither _   (Just x) = Right x

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

validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd =
  let celdas   = concat (gridNivel nd)
      numMetas = length (filter esMeta celdas)
  in if numMetas == 0
       then Left NivelSinMeta
       else if numMetas > 1
              then Left (NivelConVariasMetas numMetas)
              else Right nd


parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas = parsearFichero lineas >>= validarNivel


tamCelda :: Float
tamCelda = 40

posicionCelda :: Int -> Int -> Int -> Vector2D
posicionCelda total i j =
  let x = fromIntegral j * tamCelda
      y = fromIntegral (total - 1 - i) * tamCelda
  in (x, y)

gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
  let total = length grid
  in [ Plataforma (x, y, tamCelda, tamCelda)
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esSolido celda
     , let (x, y) = posicionCelda total i j
     ]

metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
  let total = length grid
  in [ Meta (posicionCelda total i j)
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esMeta celda
     ]

enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
  let total = length grid
  in [ Enemigo idEnemigo (posicionCelda total i j) (-40, 0) (tamCelda, tamCelda) Patrulla
     | (i, fila)  <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , InicioEnemigo idEnemigo <- [celda]
     ]


parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles niveles =
  traverse parsearNivelCompleto niveles


todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones g@(primeraFila : _) =
  (,) <$> [0 .. length g - 1]
      <*> [0 .. length primeraFila - 1]