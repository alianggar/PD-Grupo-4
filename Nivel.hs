----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- TAREA 1
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- TIPOS SINÓNIMO E IMPORTACIONES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

import Test.QuickCheck

type Vector2D = (Double,Double)
type Caja a = (a, a, a, a)
type Lado = String
type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sumaVectores :: Vector2D -> Vector2D -> Vector2D
sumaVectores (x1,y1) (x2,y2) = (x1+x2,y1+y2) 

escalarVector :: Double -> Vector2D -> Vector2D
escalarVector z (x1,y1) = (x1*z,y1*z)

distancia :: Vector2D -> Vector2D -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Caja (x,y,l,a)
-- La caja empieza en el punto (x,y)
-- y tiene largo l (eje x) y altura a (eje y)

solapan :: (Ord a, Num a) => Caja a -> Caja a -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) = 
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Calcula el lado de colisión entre dos cajas (La primera es la que colisiona con la segunda)
ladoColision :: (Ord a, Num a) => Caja a -> Caja a -> Lado
ladoColision (x1, y1, l1, a1) (x2, y2, l2, a2) 
    | minSolape == solapeArriba    = "Arriba"
    | minSolape == solapeAbajo     = "Abajo"
    | minSolape == solapeIzquierda = "Izquierda"
    | otherwise                    = "Derecha"
  where
    -- Cálculo correcto de los 4 solapes parciales
    solapeArriba    = (y2 + a2) - y1 -- (Arriba de Caja 2) - (Abajo de Caja 1)
    solapeAbajo     = (y1 + a1) - y2 -- (Arriba de Caja 1) - (Abajo de Caja 2)
    solapeIzquierda = (x1 + l1) - x2 -- (Derecha de Caja 1) - (Izquierda de Caja 2)
    solapeDerecha   = (x2 + l2) - x1 -- (Derecha de Caja 2) - (Izquierda de Caja 1)
    
    -- Valor mínimo de los solapes parciales
    minSolape = minimum [solapeArriba, solapeAbajo, solapeIzquierda, solapeDerecha]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 4
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
-- Empezamos con el caso base, si tenemos una cadena vacía, se devuelve una lista con una cadena vacía
-- y usamos el patrón ánonimo en el primer valor porque nos da igual el valor que tenga porque siempre devuelve una lista vacía
splitOn _ "" = [""]
splitOn carS (x:xs)
-- si el primer carácter es igual que el separador, significa que ha terminado la palabra que estabamos leyendoç
-- añadimos una cadena vacia al principio del resto para indicar que empieza un fragmento nuevo
    | x == carS = "" : r
-- en caso contrario, que x no es el separador, con r !! 0 accedemos al primer elemento de la lista
-- y con drop elimina el primer elemento de la lista y se queda con los restantes
    | otherwise = (x : r !! 0) : drop 1 r
    where
        r = splitOn carS xs


trim :: String -> String
trim cs = reverse (dropWhile esBlanco (reverse (dropWhile esBlanco cs)))
  where
    esBlanco c = c == ' ' || c == '\t' || c == '\n'


contarSiCumple :: (a -> Bool) -> [a] -> Int
contarSiCumple p xs = length [x | x <- xs, p x]


list2Vector2 :: [Double] -> Vector2D
list2Vector2 [] = error "*** Exception: Lista vacia no posible convertir en Vector2"
list2Vector2 [x] = error "*** Exception: Falta un elemento en la lista para convertir en Vector2"
list2Vector2 (x:y:_) = (x,y)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 5
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- '#' = solido
-- 'M' = meta
-- '.' = vacío
-- cualquier otro carácter = inicio de un enemigo
--  el propio carácter es su identificador

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


                    

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BONUS PROPIEDADS QUICKCHECK
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

prop_suma_conmutativa :: Vector2D -> Vector2D -> Bool
prop_suma_conmutativa v1 v2 = sumaVectores v1 v2 == sumaVectores v2 v1

-- *Main> quickCheck prop_suma_conmutativa
-- +++ OK, passed 100 tests.


prop_escalar_neutro :: Vector2D -> Bool
prop_escalar_neutro v = escalarVector 1 v == v

-- *Main> quickCheck prop_escalar_neutro
-- +++ OK, passed 100 tests.


prop_distancia_no_negativa :: Vector2D -> Vector2D -> Bool
prop_distancia_no_negativa v1 v2 = distancia v1 v2 >= 0


-- *Main> quickCheck prop_distancia_no_negativa
-- +++ OK, passed 100 tests.


prop_distancia_simetrica :: Vector2D -> Vector2D -> Bool
prop_distancia_simetrica v1 v2 = distancia v1 v2 == distancia v2 v1

-- *Main> quickCheck prop_distancia_simetrica
-- +++ OK, passed 100 tests.


prop_solapan_simetrica :: (Num a, Ord a) => Caja a -> Caja a -> Bool
prop_solapan_simetrica a b = solapan a b == solapan b a

-- *Main> quickCheck prop_solapan_simetrica
-- +++ OK, passed 100 tests.


prop_trim_idempotente :: String -> Bool
prop_trim_idempotente s = trim (trim s) == trim s


-- *Main> quickCheck prop_trim_idempotente
-- +++ OK, passed 100 tests.

--sep string
prop_splitOn_sin_separador :: Char -> String -> Property
prop_splitOn_sin_separador c s =
    not (c `elem` s) ==> splitOn c s == [s]


-- *Main> quickCheck prop_splitOn_sin_separador
-- +++ OK, passed 100 tests; 17 discarded.

prop_contarSiCumple_acotado :: Fun Int Bool -> [Int] -> Bool
prop_contarSiCumple_acotado (Fn p) xs = contarSiCumple p xs <= length xs

-- *Main> quickCheck prop_contarSiCumple_acotado
-- +++ OK, passed 100 tests.


{-

Inicialmente dabe error: 


<interactive>:40:1: error:
    * No instance for (Show (() -> Bool))
        arising from a use of `quickCheck'
        (maybe you haven't applied a function to enough arguments?)
    * In the expression: quickCheck prop_contarSiCumple_acotado
      In an equation for `it':
          it = quickCheck prop_contarSiCumple_acotado

QuickCheck necesita poder mostrar los argumentos cuando una prueba falla. Como la propiedad es 
polimórfica (a), GHCi elige () por defecto, y las funciones no tienen instancia de Show

Usamos Fun de QuickCheck, que genera funciones mostrables, y fijamos el tipo
-}



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- TAREA 2
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BLOQUE 1: RECURSIÓN
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1: Reimplementación recursiva de las consultas del nivel
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Funcion posicionesMetaRec que hace lo mismo que posicionesMeta de la tarea 1 
-- usando recursion y funciones auxiliares.

posicionesMetaRec :: Grid -> [(Int, Int)]
posicionesMetaRec xs = recorreFilaMeta 0 xs

recorreFilaMeta :: Int -> Grid -> [(Int, Int)]
recorreFilaMeta _ [] = []
recorreFilaMeta f (x:xs) = recorreColMeta f 0 x  ++ recorreFilaMeta (f+1) xs

recorreColMeta :: Int->Int->Fila->[(Int, Int)]
recorreColMeta _ _ [] = []
recorreColMeta f c (x:xs) 
    |esMeta x = (f,c) : recorreColMeta f (c+1) xs
    |otherwise = recorreColMeta f (c+1) xs

-- Funcion posicionesEnemigosRec que hace lo mismo que posicionesEnemigos de la tarea 1 
-- usando recursion y funciones auxiliares.

posicionesEnemigosRec :: Grid -> [(Int, Int, Celda)]
posicionesEnemigosRec xs = recorreFilaEnemigos 0 xs

recorreFilaEnemigos :: Int -> Grid -> [(Int, Int, Celda)]
recorreFilaEnemigos _ [] = []
recorreFilaEnemigos f (x:xs) = recorreColEnemigos f 0 x  ++ recorreFilaEnemigos (f+1) xs

recorreColEnemigos :: Int->Int->Fila->[(Int, Int, Celda)]
recorreColEnemigos _ _ [] = []
recorreColEnemigos f c (x:xs) 
    |esEnemigo x = (f,c,x) : recorreColEnemigos f (c+1) xs
    |otherwise = recorreColEnemigos f (c+1) xs



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 2: Generalización por recursión de agruparRachas
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3: Recursión numérica: simulación de movimiento
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Función: simularCaidaLibre
Dado un número de pasos, una aceleración (gravedad) y un estado inicial (posición, velocidad),
calcula recursivamente la lista de todos los estados por los que pasa un objeto en caída libre con
avance discreto.
> simularCaidaLibre 3 (-1) (0,0)
[(0.0,0.0),(0.0,-1.0),(-1.0,-2.0),(-3.0,-3.0)]
> simularCaidaLibre 0 (-1) (5,0)
[(5.0,0.0)]
> length (simularCaidaLibre 5 (-1) (0,0))
6
Nota
La lista resultado tiene siempre n + 1 elementos: el estado inicial más uno por cada paso
simulado.
-}

-- simularCaidaLibre pasos, aceleracion, estado inical x0
-- calcula recursivamente la lista de todos los estados por los que pasa un objeto en
-- caida libre
-- estado = (poscion,velocidad)


paso :: Double -> Vector2D -> Vector2D
paso a (p, v) = (p + v, v + a)

-- simularCaidaLibre: pasos, aceleracion, estado inicial (posicion, velocidad)
simularCaidaLibre :: Int -> Double -> Vector2D -> [Vector2D]
simularCaidaLibre 0 _ estado = [estado]
simularCaidaLibre n a estado = estado : simularCaidaLibre (n - 1) a (paso a estado)


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 4: Recursión sobre dos listas a la vez
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 5: Recursión con descarte condicional: preparación de líneas
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

esComentarioOVacia :: String -> Bool
esComentarioOVacia "" = True
esComentarioOVacia (x:_) = x == ';'

-- Limpia una lista de lneas ledas de un chero de nivel: recorta espacios en cada lnea (con
-- esComentarioOVacia para detectar comentarios, que empiezan por ’;’) y descarta las que que
-- den vacas o sean comentarios.

prepararLineas :: [String]->[String]
prepararLineas [] = []
prepararLineas (x:xs) 
    | esComentarioOVacia noEspacios = prepararLineas xs
    | otherwise = noEspacios : prepararLineas xs
    where
        noEspacios = trim x



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BLOQUE 2: FUNCIONES DE ORDEN SUPERIOR
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- SUBTAREA 5: mapPropio y filterPropio
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Funciones mapPropio y filterPropio usando recursion

mapPropio :: (a -> b) -> [a] -> [b]
mapPropio _ [] = []
mapPropio f (x:xs) = f x : mapPropio f xs

filterPropio :: (a -> Bool) -> [a] -> [a]
filterPropio _ [] = []
filterPropio f (x:xs) = if f x then x : filterPropio f xs else filterPropio f xs

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- SUBTAREA 6: foldrPropio y foldlPropio
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- SUBTAREA 7: Aplicación justificada de foldr/foldl
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Función: hayColisionConAlguna
Dada la caja delimitadora de un objeto (por ejemplo, el jugador) y una lista de cajas de
posibles obstáculos, indica si colisiona con alguna de ellas. Es el tipo de comprobación que
necesitará el sistema de colisiones de la Tarea 3, cuando el jugador tenga que comprobarse
contra todas las plataformas o enemigos del nivel a la vez.
> hayColisionConAlguna (0,0,10,10) [(20,20,5,5),(5,5,10,10)]
True
> hayColisionConAlguna (0,0,10,10) [(20,20,5,5),(50,50,5,5)]
False
> hayColisionConAlguna (0,0,10,10) []
False
Debe implementarse con foldr.
-}

--type Caja a = (a, a, a, a) x, y, ancho, largo

-- Elegimos foldr porque permite que el cortocircuito con el operador
-- || por la propia evaluacion perezosa de Haskell. Cuando se cumpla devuelve
-- True inmediatamente, dejando de evalaur el resto de la secuencia
hayColisionConAlguna :: (Ord a, Num a) => Caja a -> [Caja a] -> Bool
hayColisionConAlguna y = foldr (\x ac -> solapan x y || ac) False

-- Usamos la funcion solapan de la tare 1 que comprueba si hay colisión entre 2 cajas

{-
Función: contarCeldasQueCumplen
Cuenta cuántas celdas del nivel completo cumplen una condición dada (misma funcionalidad
que el contarCeldas que se podría haber pedido con comprensión, pero ahora resuelta con
pliegues).
> contarCeldasQueCumplen esSolido ["..M", "#.X", "###"]
4
> contarCeldasQueCumplen esVacio ["..M", "#.X", "###"]
3
Debe implementarse con foldl (una función auxiliar para contar dentro de cada la, combinada
con otra que suma los resultados de todas las las).
-}

-- Elegimos foldl, pues debemos acumular un resultado numérico de todas las filas del grid
-- sin necesidad de cortociruito, combinando la suma de las celdas en cada fila mediante
-- un recorrido sobre la secuencia de izq a derecha. 
-- La ventaja de foldl es que se realizaría en menos casos la función suma

contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
contarCeldasQueCumplen p = foldl (\ys x -> ys + cuentaCeldasQueCumplenAux p x) 0 


cuentaCeldasQueCumplenAux :: (Celda -> Bool) -> Fila -> Int
cuentaCeldasQueCumplenAux p = foldl (\ys x -> if p x then ys + 1 else ys) 0


-- CON FOLDR SERIA:

--contarCeldasQueCumplen :: (Celda -> Bool) -> Grid -> Int
--contarCeldasQueCumplen p = foldr (\x ys -> cuentaCeldasQueCumplenAux p x + ys) 0 


--cuentaCeldasQueCumplenAux :: (Celda -> Bool) -> Fila -> Int
--cuentaCeldasQueCumplenAux p = foldr (\x ys -> if p x then 1 + ys else ys) 0


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- BONUS: PROPEIDADES QUICKCHECK
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

