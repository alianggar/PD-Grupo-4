{-
## Subtarea 2 — Registros: jugador, enemigos y estado del juego (1.0 puntos)

Define los tipos de datos para:

- **jugador**: Un jugador necesita un posición, una velocidad, las vidas que le quedan 
  y un puntuación. 

- **enemigo**: Un enemigo necesita un identificador, una posición, una velocidad, una 
  dimensión y un tipo de enemigo. 

- **tipo de enemigo**: El tipo de enemigo identificará las dinámicas de los enemigos. 
  Este tipo de datos debe establecer los tipos de enemigos que tendrá el juego. Por 
  ahora establecemos dos tipos: patrulla y perseguidor.
  
- **estado del juego**: El estado de juego determina determinado eventos que puede 
  producirse durante el juego: jugando, perdida de vida, terminar (gameover), nivel
  completado, ...

Se deberán crear la siguiente funciones:

```haskell
crearJugador    :: Vector2 -> Jugador
perderVida      :: Jugador -> Jugador
sumarPuntos     :: Int -> Jugador -> Jugador
conVelocidad    :: Vector2 -> Jugador -> Jugador
moverJugador    :: Float -> Jugador -> Jugador
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
```

| Función | Puntos | Criterio |
|---|---|---|
| definición de los tipos | 0.2 | exactamente los campos y constructores indicados |
| `crearJugador` | 0.2 | jugador en la posición dada, velocidad `(0,0)`, **3 vidas** y **0 puntos** |
| `perderVida`, `sumarPuntos`, `conVelocidad` | 0.2 | uso de **actualización de registro** (`j { campo = ... }`); `perderVida` nunca deja `vidas` por debajo de 0 |
| `moverJugador` | 0.2 | `posición + dt · velocidad` reutilizando `sumaVectores` y `escalarVector` |
| `estadoSiguiente` | 0.2 | ver reglas abajo |

`estadoSiguiente jugador recibioDaño tocaMeta` recibe al jugador **ya actualizado**:

1. si `vidas jugador <= 0` → gameover
2. si no, y `recibioDaño` → pierde vida
3. si no, y `tocaMeta` → completa nivel
4. en otro caso → jugando

**Ejemplos:**

```
> let j = crearJugador (20, 200)
> j
Jugador {posJugador = (20.0,200.0), velJugador = (0.0,0.0), vidas = 3, puntuacion = 0}
> perderVida j
Jugador {posJugador = (20.0,200.0), velJugador = (0.0,0.0), vidas = 2, puntuacion = 0}
> vidas (perderVida (perderVida (perderVida (perderVida j))))
0
> sumarPuntos 50 j
Jugador {posJugador = (20.0,200.0), velJugador = (0.0,0.0), vidas = 3, puntuacion = 50}
> moverJugador 0.5 (conVelocidad (10, -20) j)
Jugador {posJugador = (25.0,190.0), velJugador = (10.0,-20.0), vidas = 3, puntuacion = 0}
> estadoSiguiente j False False
Jugando
> estadoSiguiente j True False
PierdeVida
> estadoSiguiente j False True
NivelCompletado
> estadoSiguiente (perderVida (perderVida (perderVida j))) True True
GameOver
```
-}

type Vector2D = (Float, Float)

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2,y1+y2) 

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector z (x1,y1) = (x1*z,y1*z)


data Jugador = Jugador
  { posJugador :: Vector2D
  , velJugador :: Vector2D
  , vidas      :: Int
  , puntuacion :: Int
  } deriving (Eq, Show)

data TipoEnemigo = Patrulla | Perseguidor
  deriving (Eq, Show)

data Enemigo = Enemigo
  { idEnemigo   :: String
  , posEnemigo  :: Vector2D
  , velEnemigo  :: Vector2D
  , dimEnemigo  :: Vector2D
  , tipoEnemigo :: TipoEnemigo
  } deriving (Eq, Show)

data EstadoJuego
  = Jugando
  | PierdeVida
  | GameOver
  | NivelCompletado
  deriving (Eq, Show)


-- Crea un jugador parado, con tres vidas y cero puntos.
crearJugador :: Vector2D -> Jugador
crearJugador posicionInicial = Jugador
  { posJugador = posicionInicial
  , velJugador = (0, 0)
  , vidas      = 3
  , puntuacion = 0
  }

-- Resta una vida sin bajar de cero.
perderVida :: Jugador -> Jugador
perderVida j = j { vidas = max 0 (vidas j - 1) }

-- Añade la cantidad indicada a la puntuación.
sumarPuntos :: Int -> Jugador -> Jugador
sumarPuntos n j = j { puntuacion = puntuacion j + n }

-- Establece una nueva velocidad para el jugador.
conVelocidad :: Vector2D -> Jugador -> Jugador
conVelocidad nuevaVelocidad j =
  j { velJugador = nuevaVelocidad }

-- Calcula la nueva posición según el tiempo y la velocidad.
moverJugador :: Float -> Jugador -> Jugador
moverJugador dt j =
  j { posJugador =
        sumaVectores (posJugador j)
                     (escalarVector dt (velJugador j))
    }

-- Determina el estado a partir del jugador ya actualizado.
estadoSiguiente :: Jugador -> Bool -> Bool -> EstadoJuego
estadoSiguiente j recibioDano tocaMeta
  | vidas j <= 0 = GameOver
  | recibioDano  = PierdeVida
  | tocaMeta    = NivelCompletado
  | otherwise   = Jugando