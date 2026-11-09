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

data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

data Entrada = Entrada { nombreJugador :: String, puntos :: Int } 
  deriving (Eq, Show)


trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')


serializarPartida :: Partida -> [String]
serializarPartida p =
  [ "NIVEL: " ++ show (nivelGuardado p)
  , "VIDAS: " ++ show (vidasGuardadas p)
  , "PUNTUACION: " ++ show (puntuacionGuardada p)
  ]

parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
  nivelStr  <- buscarClave "NIVEL: " lineas
  vidasStr  <- buscarClave "VIDAS: " lineas
  puntosStr <- buscarClave "PUNTUACION: " lineas
  nivel     <- readMaybe (trim nivelStr)
  vidas     <- readMaybe (trim vidasStr)
  puntos    <- readMaybe (trim puntosStr)
  return (Partida nivel vidas puntos)

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

parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas = mapM parsearEntrada (filter (not . null . trim) lineas)

mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n entradas = take n (sortOn (\e -> Down (puntos e)) entradas)

parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
  Partida
    <$> (buscarClave "NIVEL: " lineas >>= readMaybe . trim)
    <*> (buscarClave "VIDAS: " lineas >>= readMaybe . trim)
    <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe . trim)

puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas =
  (sum . map puntos) <$> parsearLeaderboard lineas