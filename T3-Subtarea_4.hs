import Data.List (stripPrefix, sortOn)
import Data.Ord (Down(..))
import Text.Read (readMaybe)
import Control.Monad (mapM)

-- Tipos definidos en el enunciado
data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

data Entrada = Entrada { nombreJugador :: String, puntos :: Int } 
  deriving (Eq, Show)

-- Función proporcionada en el enunciado para dividir cadenas
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, b)  -> a : splitOn c (tail b)

-- Función auxiliar para eliminar espacios a los lados
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- Devuelve lo que sigue a la clave en la primera línea que empiece por ella
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case stripPrefix clave x of
  Just resto -> Just resto
  Nothing    -> buscarClave clave xs

-- Convierte los datos de una partida en una lista de cadenas de texto
serializarPartida :: Partida -> [String]
serializarPartida p =
  [ "NIVEL: " ++ show (nivelGuardado p)
  , "VIDAS: " ++ show (vidasGuardadas p)
  , "PUNTUACION: " ++ show (puntuacionGuardada p)
  ]

-- Analiza una partida en notación do sobre Maybe, fallando con Nothing si falta algo
parsearPartida :: [String] -> Maybe Partida
parsearPartida lineas = do
  nivelStr  <- buscarClave "NIVEL: " lineas
  vidasStr  <- buscarClave "VIDAS: " lineas
  puntosStr <- buscarClave "PUNTUACION: " lineas
  nivel     <- readMaybe (trim nivelStr)
  vidas     <- readMaybe (trim vidasStr)
  puntos    <- readMaybe (trim puntosStr)
  return (Partida nivel vidas puntos)

-- Parsea el formato "nombre; puntos", verificando que el nombre no sea vacío
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

-- Parsea una lista de entradas con mapM ignorando las líneas en blanco
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas = mapM parsearEntrada (filter (not . null . trim) lineas)

-- Devuelve las n mejores entradas ordenadas de mayor a menor puntuación
mejoresPuntuaciones :: Int -> [Entrada] -> [Entrada]
mejoresPuntuaciones n entradas = take n (sortOn (\e -> Down (puntos e)) entradas)