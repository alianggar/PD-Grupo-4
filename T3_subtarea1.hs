{-
> **Compatibilidad con las Tareas 1 y 2.** Sustituís `type Celda = Char` por un tipo
> `data`. Las funciones que clasificaban celdas (`esSolido`, `esMeta`, `esEnemigo`) pasan
> a hacer *pattern matching* sobre los constructores. Si el resto de vuestro código de las
> tareas anteriores usaba `Char`, adaptadlo o dejadlo fuera: **solo se evalúa lo que se
> pide en este enunciado**.

# Bloque 1 — Definición de tipos (3.0 puntos)

## Subtarea 1 — `Celda` como tipo algebraico (1.0 puntos)

Tomamos ya la decisión que se anunciaba en la Tarea 1: una celda deja de ser un `Char`.
Ahora una celda será un tipo de datos `data Celda = ...` con los constructures necesarios para
contemplar las casuísticas que hay en un mapa.

```haskell
parsearCelda :: Char -> Celda
parsearGrid  :: [String] -> Grid
celdaAChar   :: Celda -> Char
esSolido     :: Celda -> Bool
esMeta       :: Celda -> Bool
esEnemigo    :: Celda -> Bool
```

| Función | Puntos | Criterio |
|---|---|---|
| `parsearCelda` y `parsearGrid` | 0.25 | Convierte un carácter al tipo de celda correspondiente |
| `celdaAChar` | 0.25 | operación inversa de `parsearCelda` |
| `esSolido`, `esMeta`, `esEnemigo` | 0.5 | Determina si una Celda es alguno de estos tipos |

**Ejemplos:**

```
> parsearCelda '#'
Solido
> parsearCelda 'A'
InicioEnemigo "A"
> parsearGrid ["#.M", "A.."]
[[Solido,Vacio,META],[InicioEnemigo "A",Vacio,Vacio]]
> map (map celdaAChar) (parsearGrid ["#.M", "A.."])
["#.M","A.."]
> map esSolido [Solido, Vacio, META, InicioEnemigo "A"]
[True,False,False,False]
```
-}

data Celda = Solido | Vacio | META | InicioEnemigo String
  deriving (Eq, Show)

type Grid = [[Celda]]

-- Convierte un caracter del mapa en una celda.
parsearCelda :: Char -> Celda
parsearCelda '#' = Solido
parsearCelda '.' = Vacio
parsearCelda 'M' = META
-- Cualquier otro simbolo identifica un enemigo.
parsearCelda c = InicioEnemigo [c]

-- Convierte todas las filas del mapa en listas de celdas.
parsearGrid :: [String] -> Grid
parsearGrid filas = map (map parsearCelda) filas

-- Recupera el caracter de una celda; un identificador vacio se representa por '.'.
celdaAChar :: Celda -> Char
celdaAChar Solido = '#'
celdaAChar Vacio = '.'
celdaAChar META = 'M'
celdaAChar (InicioEnemigo (c:_)) = c
celdaAChar (InicioEnemigo []) = '.'

-- Indica si la celda es solida.
esSolido :: Celda -> Bool
esSolido Solido = True
esSolido _ = False

-- Indica si la celda es la meta.
esMeta :: Celda -> Bool
esMeta META = True
esMeta _ = False

-- Indica si la celda contiene el inicio de un enemigo.
esEnemigo :: Celda -> Bool
esEnemigo (InicioEnemigo _) = True
esEnemigo _ = False