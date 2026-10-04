
{-
1 Vectores 2D 

Puntuación:
0.3 pts: Denición correcta del tipo sinónimo para un punto/vector 2D, usado de forma
consistente en las tres funciones
0.4 pts: sumaVectores
0.4 pts: escalarVector

-}

type Vector2D = (Double,Double)

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2,y1+y2) 

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector z (x1,y1) = (x1*z,y1*z)

distancia :: Vector2D -> Vector2D -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)



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

-- PREGUNTAR PROFESOR FORMATO CAJA -> CAJA = (posicion, tamano) -> ((Double,Double),  (Double,Double))
type Caja a = (a, a, a, a)

solapan :: (Ord a, Num a) => Caja a -> Caja a -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) = 
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)


{-


3) Lado de colisión (1.5 puntos)
Dadas dos cajas que ya colisionan, se pide determinar por qué lado es más supercial el
solape (ese es el lado por el que efectivamente se produjo el contacto).
Puntuación:
0.7 pts: Cálculo correcto de los 4 solapes parciales (usando where)
0.8 pts: Guardas que eligen el lado correcto en todos los ejemplos
Función: ladoColision
Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el de
menor solape).
-}

-- Caja (x,y,l,a)
-- La caja empieza en el punto (x,y)
-- y tiene largo l (eje x) y altura a (eje y)

-- Definición del tipo de dato para el resultado de la colisión
data Lado = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

-- Definición del tipo sinónimo para las cajas (x, y, largo, alto). 


-- Calcula el lado de colisión entre dos cajas (La primera es la que colisiona con la segunda)
ladoColision :: (Ord a, Num a) => Caja a -> Caja a -> Lado
ladoColision (x1, y1, l1, a1) (x2, y2, l2, a2) 
    | minSolape == solapeArriba    = Arriba
    | minSolape == solapeAbajo     = Abajo
    | minSolape == solapeIzquierda = Izquierda
    | otherwise                    = Derecha
  where
    -- Cálculo correcto de los 4 solapes parciales
    solapeArriba    = (y2 + a2) - y1 -- (Arriba de Caja 2) menos (Abajo de Caja 1)
    solapeAbajo     = (y1 + a1) - y2 -- (Arriba de Caja 1) - (Abajo de Caja 2)
    solapeIzquierda = (x1 + l1) - x2 -- (Derecha de Caja 1) - (Izquierda de Caja 2)
    solapeDerecha   = (x2 + l2) - x1 -- (Derecha de Caja 2) - (Izquierda de Caja 1)
    
    -- Valor mínimo de los solapes parciales
    minSolape = minimum [solapeArriba, solapeAbajo, solapeIzquierda, solapeDerecha]

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


                        

{-
4 Utilidades de listas y cadenas (2 puntos)
Puntuación:
0.6 splitOn (recursión vía pattern matching x:xs; no usar Data.List.splitOn)
0.4 trim
0.5 contarSiCumple, resuelta obligatoriamente con lista por comprensión, no con foldr/map
0.5 list2Vector2, con pattern matching en los casos [], [x], (x:y:_) y uso de error en los
casos inválidos; el tipo del resultado debe coincidir con el denido en la Subtarea 1
-}

splitOn :: Char -> String -> [String]
splitOn c = aux ""
  where
    aux palabra [] = [reverse palabra]
    aux palabra (x:xs)
      | x == c    = reverse palabra : aux "" xs
      | otherwise = aux (x:palabra) xs


trim :: String -> String
trim cs = reverse (dropWhile esBlanco (reverse (dropWhile esBlanco cs)))
  where
    esBlanco c = c == ' ' || c == '\t' || c == '\n'


contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple _ [] = 0
contarSiCumple p (x:xs)
    | p x   = 1 + contarSiCumple p xs
    | otherwise = contarSiCumple p xs


list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [x] = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)


-- BONUS PROPIEDADS QUICKCHECK

{-






-}























