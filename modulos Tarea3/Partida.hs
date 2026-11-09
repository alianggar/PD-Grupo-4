module Partida
  ( Partida(..), Entrada(..)
  , serializarPartida, parsearPartida
  , parsearEntrada, parsearLeaderboard
  , mejoresPuntuaciones
  , parsearPartidaA, puntuacionTotal
  ) where

import Nivel (splitOn, buscarClave)
import qualified Data.List as L
import Data.Ord (Down(..))
import Text.Read (readMaybe)

-- Partida guardada: nivel alcanzado, vidas y puntuación.
data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

-- Entrada de la tabla de puntuaciones: nombre del jugador y sus puntos.
data Entrada = Entrada { nombreJugador :: String, puntos :: Int }
  deriving (Eq, Show)

-- Quita los espacios al principio y al final de una cadena.
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- Convierte una partida en las líneas de texto de su fichero de guardado.
serializarPartida :: Partida -> [String]
serializarPartida p =
  [ "NIVEL: " ++ show (nivelGuardado p)
  , "VIDAS: " ++ show (vidasGuardadas p)
  , "PUNTUACION: " ++ show (puntuacionGuardada p)
  ]

-- Interpreta las líneas de una partida guardada, o Nothing si falta una clave o un número es ilegible.
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
  nivelStr  <- buscarClave "NIVEL: " lineas
  vidasStr  <- buscarClave "VIDAS: " lineas
  puntosStr <- buscarClave "PUNTUACION: " lineas
  nivel     <- readMaybe (trim nivelStr)
  vidas     <- readMaybe (trim vidasStr)
  puntos    <- readMaybe (trim puntosStr)
  return (Partida nivel vidas puntos)

-- Interpreta una línea "nombre;puntos" como entrada, o Nothing si el formato no es válido.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea = do
  let partes = splitOn ';' linea
  if length partes == 2
    then do
      let nom = trim (partes !! 0)
          ptsStr = trim (partes !! 1)
      if null nom
        then Nothing
        else do
          p <- readMaybe ptsStr
          return (Entrada nom p)
    else Nothing

-- Interpreta una tabla de puntuaciones ignorando líneas vacías, o Nothing si alguna otra línea es inválida.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas = mapM parsearEntrada (filter (not . null . trim) lineas)

-- Devuelve las n mejores entradas, de mayor a menor puntuación.
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n entradas = take n (sortOn (\e -> Down (puntos e)) entradas)

-- Igual que parsearPartida pero construyendo la partida con <$> y <*> (bonus Applicative).
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
  Partida
    <$> (buscarClave "NIVEL: " lineas >>= readMaybe . trim)
    <*> (buscarClave "VIDAS: " lineas >>= readMaybe . trim)
    <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe . trim)

-- Suma los puntos de una tabla de puntuaciones válida, o Nothing si no lo es (bonus Functor).
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas =
  (sum . map puntos) <$> parsearLeaderboard lineas