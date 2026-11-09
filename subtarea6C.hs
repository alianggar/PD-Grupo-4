import Graphics.Gloss.Interface.Pure.Game
import Data.List (stripPrefix, sortOn)
import Text.Read (readMaybe)

-- ============================================================
-- TIPOS Y FUNCIONES BASE (módulo Tipos)
-- ============================================================

type BoundingBox = (Float, Float, Float, Float)
type Vector2D    = (Float, Float)

-- Multiplica un vector por un escalar.
escalarVector :: Float -> Vector2D -> Vector2D
escalarVector z (x1, y1) = (x1 * z, y1 * z)

-- Suma dos vectores componente a componente.
sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

data EstadoJuego
  = Jugando
  | PierdeVida
  | GameOver
  | NivelCompletado
  deriving (Eq, Show)

-- Funciones necesarias de la subtarea1

data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
parsearCelda c   = InicioEnemigo [c]

parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _      = False

esMeta :: Celda -> Bool
esMeta META = True
esMeta _    = False

esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _                 = False



-- Funciones necesarias de la subtarea 2

data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

crearJugador :: Vector2D -> Jugador
crearJugador posicionInicial = Jugador
  { posJugador = posicionInicial
  , velJugador = (0, 0)
  , vidas      = 3
  , puntuacion = 0
  }

perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }


sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad nuevaVelocidad j =
  j { velJugador = nuevaVelocidad }

moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j =
  j { posJugador =
        sumaVectores (posJugador j)
                     (escalarVector dt (velJugador j))
    }


estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDano  = PierdeVida
  | tocaMeta     = NivelCompletado
  | otherwise    = Jugando


--Funciones necesarias de la subtarea3

class Objeto a where
    posicion    :: a -> Vector2D
    boundingBox :: a -> BoundingBox
    animar      :: Float -> a -> a
    animar _ = id

data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
    deriving (Eq, Show)

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
        in (x - w/2, y - h/2, w, h)
    animar dt e =
        let desp     = escalarVector dt (velEnemigo e)
            nuevaPos = sumaVectores (posEnemigo e) desp
        in e { posEnemigo = nuevaPos }

solapan :: BoundingBox -> BoundingBox -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) =
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)

colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)

colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a xs = any (colisionaCon a) xs


--Funciones necesarias de la subtarea 4

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])  -> [a]
  (a, b)   -> a : splitOn c (tail b)

buscarClave :: String -> [String] -> Maybe String
buscarClave _ [] = Nothing
buscarClave clave (x:xs) = case stripPrefix clave x of
  Just resto -> Just resto
  Nothing    -> buscarClave clave xs


--Funciones necesarias subtarea5

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


--Funcione necesarias subtarea 6A

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



-- SUBTAREA 6C -- Elementos: Mundo, mundoInicial, actualizarMundo, manejarEvento


-- Estado completo del juego: jugador, enemigos, plataformas, metas, estado y nivel.
data Mundo = Mundo
  { jugadorM     :: Jugador
  , enemigosM    :: [Enemigo]
  , plataformasM :: [Plataforma]
  , metasM       :: [Meta]
  , estadoM      :: EstadoJuego
  , nivelM       :: NivelData
  } deriving (Eq, Show)

-- Construye el mundo de partida a partir de los datos de un nivel.
mundoInicial :: NivelData -> Mundo
mundoInicial nd = Mundo
  { jugadorM     = crearJugador (60, 120)
  , enemigosM    = enemigosDesdeGrid g
  , plataformasM = gridAPlataformas g
  , metasM       = metasDesdeGrid g
  , estadoM      = Jugando
  , nivelM       = nd
  }
  where g = gridNivel nd

-- Avanza la simulación dt segundos: mueve jugador y enemigos, detecta daño y meta y actualiza el estado.
actualizarMundo :: Float -> Mundo -> Mundo
actualizarMundo dt m
    | estadoM m /= Jugando = m
    | otherwise =
        let jug              = jugadorM m
            enemigos         = enemigosM m
            metas            = metasM m
            jugMovido        = moverJugador dt jug
            enemigosAnimados = map (animar dt) enemigos
            tocaEnemigo      = colisionaConAlguno jugMovido enemigosAnimados
            tocaMeta         = colisionaConAlguno jugMovido metas
            jugFinal         = if tocaEnemigo then perderVida jugMovido else jugMovido
            nuevoEstado      = estadoSiguiente jugFinal tocaEnemigo tocaMeta
        in m { jugadorM  = jugFinal
             , enemigosM = enemigosAnimados
             , estadoM   = nuevoEstado
             }

-- Traduce las flechas del teclado en velocidad del jugador y la tecla 'r' en reinicio del nivel.
manejarEvento :: Event -> Mundo -> Mundo
-- Flecha derecha pulsada: velocidad horizontal 100.
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (100, vy) (jugadorM m) }
-- Flecha izquierda pulsada: velocidad horizontal -100.
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (-100, vy) (jugadorM m) }
-- Flecha derecha soltada: anula la velocidad horizontal.
manejarEvento (EventKey (SpecialKey KeyRight) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }
-- Flecha izquierda soltada: anula la velocidad horizontal.
manejarEvento (EventKey (SpecialKey KeyLeft) Up _ _) m =
    let (_, vy) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (0, vy) (jugadorM m) }
-- Flecha arriba pulsada: velocidad vertical 100.
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 100) (jugadorM m) }
-- Flecha abajo pulsada: velocidad vertical -100.
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, -100) (jugadorM m) }
-- Flecha arriba soltada: anula la velocidad vertical.
manejarEvento (EventKey (SpecialKey KeyUp) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }
-- Flecha abajo soltada: anula la velocidad vertical.
manejarEvento (EventKey (SpecialKey KeyDown) Up _ _) m =
    let (vx, _) = velJugador (jugadorM m)
    in m { jugadorM = conVelocidad (vx, 0) (jugadorM m) }
-- Tecla 'r' pulsada: reinicia el nivel actual.
manejarEvento (EventKey (Char 'r') Down _ _) m =
    mundoInicial (nivelM m)
-- Cualquier otro evento: el mundo no cambia.
manejarEvento _ m = m