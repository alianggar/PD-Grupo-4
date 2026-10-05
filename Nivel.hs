----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 1
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

import Test.QuickCheck

type Vector2D = (Double,Double)

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

type Caja a = (a, a, a, a)

solapan :: (Ord a, Num a) => Caja a -> Caja a -> Bool
solapan (x1, y1, l1, a1) (x2, y2, l2, a2) = 
    (x1 < x2 + l2) && (x2 < x1 + l1) && (y1 < y2 + a2) && (y2 < y1 + a1)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- EJERCICIO 3
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Caja (x,y,l,a)
-- La caja empieza en el punto (x,y)
-- y tiene largo l (eje x) y altura a (eje y)

type Lado = String

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





