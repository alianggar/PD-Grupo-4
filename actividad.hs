{-  2 Cajas de colisión (2 puntos)
Puntuación:
0.5 pts: Definición correcta del tipo sinónimo para una caja delimitadora, coherente con
los ejemplos (4 componentes: posición y tamaño)
1.0 pts: Condición de solape correcta en ambos ejes
0.5 pts: Casos límite (cajas que solo se tocan en el borde, sin solaparse) resueltos igual que
en los ejemplos
Función: solapan
Indica si dos cajas delimitadoras, alineadas con los ejes, se solapan en algún punto.-}


--type Posicion = (Double, Double)
--type Tamaño = (Double, Double)
--type Caja = (Posicion, Tamaño)


-- Caja (x,y,l,a)
-- La caja empieza en el punto (x,y)
-- y tiene largo l (eje x) y altura a (eje y)

type Caja a = (a, a, a, a)

solapan :: (Ord a, Num a) => Caja a -> Caja a -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) = 
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)


{-

5 Parseo del nivel (3 puntos)
El nivel se representa como una lista de las de texto, siguiendo el convenio: '#' = sólido,
'M' = meta, '.' = vacío, cualquier otro carácter = inicio de un enemigo (el propio carácter es
su identicador).
Debéis decidir vosotros los tipos sinónimo para representar una celda y el nivel completo
(Grid), coherentes con los ejemplos de ejecución de todas las funciones de esta subtarea.
Puntuación:
0.4 pts: Tipos sinónimo para celda y nivel, coherentes en todas las funciones de la subtarea
0.4 pts: esSolido, esMeta, esVacio (pattern matching directo sobre el carácter)
0.3 pts: esEnemigo, denida en términos de las tres anteriores (sin repetir lógica)
0.4 pts: posicionesMeta (comprensión anidada con zip y guarda)
0.4 pts: posicionesEnemigos (análoga, extrayendo el identicador)
0.8 pts: agruparRachas: uso correcto de span/recursión con acumulador de índice, y manejo
correcto de rachas al nal de la la o las sin sólidos
0.3 pts: Estilo: función auxiliar de agruparRachas en where, bien nombrada y tipada
Función: parsearNivel
Convierte la lista de líneas de texto leídas de un chero de nivel en la estructura de datos
que representa el nivel completo

-}

-- '#' = solido
-- 'M' = meta
-- '.' = vacío
-- cualquier otro carácter = inicio de un enemigo
--      el propio carácter es su identificador

type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

parsearNivel :: [String] -> Grid
parsearNivel css  = css

esSolido :: Celda -> Bool
esSolido c = c == '#'

esMeta :: Celda -> Bool
esMeta c = c == 'M'

esVacio :: Celda -> Bool
esVacio c = c == '.'

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

posicionesMeta :: Grid -> [(Int,Int)]
posicionesMeta xs = [(r,i) | (r,x) <- zip [0..] xs, (i,y) <- zip [0..] x, esMeta y]

-- ¿ERROR CELDA? 

posicionesEnemigos :: Grid -> [(Int,Int,Celda)]
posicionesEnemigos xs = [(r,i,y) | (r,x) <- zip [0..] xs, (i,y) <- zip [0..] x, esEnemigo y]

-- La longitud de la racha es el final - inicio de una racha + 1
-- Si teneos ".###..."
-- inicio = 1
-- final = 3
-- resultado = (3-1) +1 = 3

-- Para una rcha emepezamso en i y terminamos en j
agruparRachas :: Fila -> [(Int,Int)]
agruparRachas xs = [(x,y-x+1) | (x,y) <- zip (inicioRachas xs) (finRachas xs) ]

inicioRachas :: String -> [Int]
inicioRachas xs = [ i | i <- [0 .. length xs - 1]
                       , esSolido (xs !! i)
                       , i == 0 || not (esSolido (xs !! (i - 1)))]

finRachas :: Fila -> [Int]
finRachas xs = [j | j <- [0..length xs -1]
                        , esSolido (xs !! j)
                        , j == length xs -1 || not (esSolido (xs !! (j + 1)))]





