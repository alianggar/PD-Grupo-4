{-
# Bonus — `Functor`, `Applicative` y `Monad` (hasta +1.0 punto extra)

Puntuación: 0.25 puntos por función, hasta un máximo de 1.0. Se trata de **reescribir en
estilo funcional** código que ya sabéis hacer con `case`/`do`, usando las operaciones de
las clases `Functor`, `Applicative` y `Monad`/`Traversable` (`fmap`, `<$>`, `<*>`, `pure`,
`>>=`, `traverse`…). **Prohibido usar `case` y `do`** en estas cuatro funciones.

```haskell
parsearPartidaA    :: [String] -> Maybe Partida          -- módulo Partida
puntuacionTotal    :: [String] -> Maybe Int              -- módulo Partida
parsearNiveles     :: [[String]] -> Either ErrorNivel [NivelData]   -- módulo Nivel
todasLasPosiciones :: Grid -> [(Int, Int)]               -- módulo Nivel
```

| Función | Puntos | Qué se pide |
|---|---|---|
| `parsearPartidaA` | 0.25 | **`Applicative`**: igual que `parsearPartida` (Subtarea 4), pero construyendo `Partida` con `<$>` y `<*>` en lugar de `do`. Debe dar exactamente los mismos resultados |
| `puntuacionTotal` | 0.25 | **`Functor`**: suma los puntos de una tabla de puntuaciones válida (`parsearLeaderboard`), aplicando una función **dentro** del `Maybe` con `<$>`/`fmap` |
| `parsearNiveles` | 0.25 | **`traverse`** (o `mapM`) sobre `Either`: interpreta y valida una lista de niveles con `parsearNivelCompleto`; devuelve todos los niveles o **el primer error** |
| `todasLasPosiciones` | 0.25 | **`Applicative` de listas**: todas las posiciones `(fila, columna)` de una rejilla rectangular, en orden de filas, con `(,) <$> ... <*> ...` y sin comprensiones |

**Ejemplos:**

```
> parsearPartidaA ["NIVEL: 2", "VIDAS: 3", "PUNTUACION: 150"]
Just (Partida {nivelGuardado = 2, vidasGuardadas = 3, puntuacionGuardada = 150})
> parsearPartidaA ["NIVEL: 2", "VIDAS: tres", "PUNTUACION: 150"]
Nothing
> parsearPartidaA ["NIVEL: 2", "VIDAS: 3"]
Nothing
> puntuacionTotal ["Ana;120", "Luis;340", "Eva;200"]
Just 660
> puntuacionTotal []
Just 0
> puntuacionTotal ["Ana;120", "roto"]
Nothing
> parsearNiveles [["NOMBRE: A", "FONDOS: 1", "---", "..M"], ["NOMBRE: B", "FONDOS: 2", "---", "#.M"]]
Right [NivelData {nombreNivel = "A", fondos = [1], gridNivel = [[Vacio,Vacio,META]]},NivelData {nombreNivel = "B", fondos = [2], gridNivel = [[Solido,Vacio,META]]}]
> parsearNiveles [["NOMBRE: A", "FONDOS: 1", "---", "..M"], ["NOMBRE: B", "FONDOS: 2", "---", "..."]]
Left NivelSinMeta
> parsearNiveles []
Right []
> todasLasPosiciones (parsearGrid ["..", "#."])
[(0,0),(0,1),(1,0),(1,1)]
> todasLasPosiciones (parsearGrid ["..M"])
[(0,0),(0,1),(0,2)]
> todasLasPosiciones []
[]
```
-}

import Data.List (stripPrefix)
import Text.Read (readMaybe)

-- ============================================================
-- TIPOS NECESARIOS
-- ============================================================

data Celda
  = Solido
  | Vacio
  | META
  | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

data Partida = Partida
  { nivelGuardado      :: Int
  , vidasGuardadas     :: Int
  , puntuacionGuardada :: Int
  } deriving (Eq, Show)

data Entrada = Entrada
  { nombreJugador :: String
  , puntos        :: Int
  } deriving (Eq, Show)

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


-- ============================================================
-- FUNCIONES NECESARIAS DE LA SUBTAREA 1
-- ============================================================

-- Convierte un carácter del mapa en una celda.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

-- Convierte las filas de texto en una rejilla de celdas.
parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

-- Indica si una celda es una meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False


-- ============================================================
-- FUNCIONES AUXILIARES COMPARTIDAS
-- ============================================================

-- Divide una cadena por el separador indicado.
splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, _:b) -> a : splitOn c b

-- Elimina los espacios al principio y al final de una cadena.
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Busca la primera línea que empieza por la clave y devuelve el resto.
buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (linea : resto) =
  case stripPrefix clave linea of
    Just valor -> Just valor
    Nothing    -> buscarClave clave resto


-- ============================================================
-- FUNCIONES NECESARIAS DE LA SUBTAREA 4
-- ============================================================

-- Interpreta una entrada con formato nombre;puntos.
parsearEntrada :: String -> Maybe Entrada
parsearEntrada linea =
  case splitOn ';' linea of
    [nombre, textoPuntos]
      | not (null (trim nombre)) -> do
          cantidad <- readMaybe (trim textoPuntos)
          return (Entrada (trim nombre) cantidad)
    _ -> Nothing

-- Interpreta la tabla completa ignorando las líneas en blanco.
parsearLeaderboard :: [String] -> Maybe [Entrada]
parsearLeaderboard lineas =
  mapM parsearEntrada (filter (not . null . trim) lineas)


-- ============================================================
-- FUNCIONES NECESARIAS DE LA SUBTAREA 5
-- ============================================================

-- Convierte un Maybe en Either añadiendo un mensaje si falta el valor.
auxEither :: String -> Maybe a -> Either ErrorNivel a
auxEither mensaje Nothing =
  Left (FormatoInvalido mensaje)
auxEither _ (Just valor) =
  Right valor

-- Interpreta la cabecera y el mapa de un nivel.
parsearFichero :: [String] -> Either ErrorNivel NivelData
parsearFichero lineas = do
  let (cabecera, resto) = break (== "---") lineas

  mapaLineas <- case resto of
    [] ->
      Left (FormatoInvalido "falta el separador '---'")
    [_] ->
      Left (FormatoInvalido "falta el mapa del nivel")
    (_ : mapa) ->
      Right mapa

  nombre <- auxEither
    "falta la clave 'NOMBRE: '"
    (buscarClave "NOMBRE: " cabecera)

  textoFondos <- auxEither
    "falta la clave 'FONDOS: '"
    (buscarClave "FONDOS: " cabecera)

  listaFondos <- auxEither
    "los fondos no son numéricos"
    (mapM readMaybe (splitOn ',' (trim textoFondos)))

  return (NivelData
    (trim nombre)
    listaFondos
    (parsearGrid mapaLineas))

-- Comprueba que un nivel contiene exactamente una meta.
validarNivel :: NivelData -> Either ErrorNivel NivelData
validarNivel nivel
  | numeroMetas == 0 = Left NivelSinMeta
  | numeroMetas > 1  = Left (NivelConVariasMetas numeroMetas)
  | otherwise       = Right nivel
  where
    numeroMetas = length (filter esMeta (concat (gridNivel nivel)))

-- Interpreta un nivel y después comprueba su validez.
parsearNivelCompleto :: [String] -> Either ErrorNivel NivelData
parsearNivelCompleto lineas =
  parsearFichero lineas >>= validarNivel


-- ============================================================
-- BONUS: SIN CASE NI DO EN ESTAS CUATRO FUNCIONES
-- ============================================================

-- Construye una partida con Applicative sobre Maybe.
parsearPartidaA :: [String] -> Maybe Partida
parsearPartidaA lineas =
  Partida
    <$> (buscarClave "NIVEL: " lineas >>= readMaybe . trim)
    <*> (buscarClave "VIDAS: " lineas >>= readMaybe . trim)
    <*> (buscarClave "PUNTUACION: " lineas >>= readMaybe . trim)

-- Suma las puntuaciones dentro del Maybe de una tabla válida.
puntuacionTotal :: [String] -> Maybe Int
puntuacionTotal lineas =
  (sum . map puntos) <$> parsearLeaderboard lineas

-- Interpreta todos los niveles o devuelve el primer error.
parsearNiveles :: [[String]] -> Either ErrorNivel [NivelData]
parsearNiveles niveles =
  traverse parsearNivelCompleto niveles

-- Genera todas las posiciones de una rejilla rectangular por filas.
todasLasPosiciones :: Grid -> [(Int, Int)]
todasLasPosiciones [] = []
todasLasPosiciones g@(primeraFila : _) =
  (,) <$> [0 .. length g - 1]
      <*> [0 .. length primeraFila - 1]