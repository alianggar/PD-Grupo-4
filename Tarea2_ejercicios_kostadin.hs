-- Bloque 1 -Apartado 1 Reimplementación recursiva de las consultas del nivel.
-- Para este apartado he usado los tipos y las funciones esMeta y esEnemigo 
-- que ya fueron implementadas en la Tarea1

type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

esMeta :: Celda -> Bool
esMeta c = c == 'M'

esSolido :: Celda -> Bool
esSolido c = c == '#'

esVacio :: Celda -> Bool
esVacio c = c == '.'

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

--Funcion posicionesMetaRec usando recursion y funciones auxiliares.

posicionesMetaRec::Grid -> [(Int, Int)]
posicionesMetaRec xs = recorreFilaMeta 0 xs

recorreFilaMeta::Int -> Grid -> [(Int, Int)]
recorreFilaMeta _ [] = []
recorreFilaMeta f (x:xs) = recorreColMeta f 0 x  ++ recorreFilaMeta (f+1) xs

recorreColMeta::Int->Int->Fila->[(Int, Int)]
recorreColMeta _ _ [] = []
recorreColMeta f c (x:xs) 
    |esMeta x = (f,c) : recorreColMeta f (c+1) xs
    |otherwise = recorreColMeta f (c+1) xs


--Funcion posicionesEnemigosRec usando recursion y funciones auxiliares.

posicionesEnemigosRec::Grid -> [(Int, Int, Celda)]
posicionesEnemigosRec xs = recorreFilaEnemigos 0 xs

recorreFilaEnemigos::Int -> Grid -> [(Int, Int, Celda)]
recorreFilaEnemigos _ [] = []
recorreFilaEnemigos f (x:xs) = recorreColEnemigos f 0 x  ++ recorreFilaEnemigos (f+1) xs

recorreColEnemigos::Int->Int->Fila->[(Int, Int, Celda)]
recorreColEnemigos _ _ [] = []
recorreColEnemigos f c (x:xs) 
    |esEnemigo x = (f,c,x) : recorreColEnemigos f (c+1) xs
    |otherwise = recorreColEnemigos f (c+1) xs

