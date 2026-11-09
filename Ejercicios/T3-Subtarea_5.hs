import Data.List (stripPrefix, sortOn)
import Text.Read (readMaybe)

-- Tipos predefinidos (Subtarea 1)
data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

-- Funciones predefinidas (Subtarea 1)
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c = InicioEnemigo [c]

parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- Funciones auxiliares (Subtarea 4)
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, b)  -> a : splitOn c (tail b)

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ') 

-- COMIENZO SUBTAREA 5 --------------------------------

-- Tipos definidos en el enunciado
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

buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case stripPrefix clave x of
  Just resto -> Just resto
  Nothing    -> buscarClave clave xs

-- Función auxiliar para inyectar fallos de Maybe dentro de la mónada Either
auxEither :: String -> Maybe a -> Either ErrorNivel a
auxEither msg Nothing  = Left (FormatoInvalido msg)
auxEither _   (Just x) = Right x

-- Procesa el texto del nivel usando do sobre Either con errores específicos
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
          nom <- auxEither "Falta la clave NOMBRE:" (buscarClave "NOMBRE: " cabecera)
          fdsStr <- auxEither "Falta la clave FONDOS:" (buscarClave "FONDOS: " cabecera)
          let fdsText = splitOn ',' (trim fdsStr)
          fds <- auxEither "Los fondos no son numéricos" (mapM readMaybe fdsText)
          let grid = parsearGrid mapaLineas 
          return (NivelData (trim nom) fds grid)

-- Verifica que el nivel tenga exactamente una meta
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nd =
  let celdas = concat (gridNivel nd)
      numMetas = length (filter esMeta celdas) 
  in if numMetas == 0
       then Left NivelSinMeta
       else if numMetas > 1
              then Left (NivelConVariasMetas numMetas)
              else Right nd

-- Compone el parseo y la validación usando bind (>>=) sin sentencias case anidadas
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas = parsearFichero lineas >>= validarNivel