{- Ejercicio 2 Generalización por recursión de agruparRachas (1.5 puntos)

Puntuación:
1.5 pts: agruparPorCaracter

Función: agruparPorCaracter
Recorre una fila y agrupa en tríos (columna de inicio, longitud, carácter) 
las rachas de caracteres consecutivos iguales. -}

type Celda = Char
type Fila = [Celda]

agruparPorCaracter :: Fila -> [(Int, Int, Char)]
agruparPorCaracter cadena = agruparAux cadena 0

agruparAux :: Fila -> Int -> [(Int, Int, Char)]
agruparAux [] _ = []
agruparAux (x:xs) indiceActual =
    let (longitud, resto) = contarRacha x xs 1
    in (indiceActual, longitud, x) : agruparAux resto (indiceActual + longitud)

contarRacha :: Char -> Fila -> Int -> (Int, Fila)
contarRacha _ [] contador = (contador, [])
contarRacha c (x:xs) contador
    | c == x    = contarRacha c xs (contador + 1)
    | otherwise = (contador, (x:xs))


{- Subtarea 6: foldrPropio y foldlPropio

Puntuación:
0.75 pts: foldrPropio
0.75 pts: foldlPropio

Función: foldrPropio
Combina los elementos de una lista de derecha a izquierda usando una función binaria y un
valor inicial. No podéis usar foldr de la librería para definirla.

Función: foldlPropio
Combina los elementos de una lista de izquierda a derecha, arrastrando un acumulador que
empieza en el valor inicial. No podéis usar foldl de la librería para definirla. -}

foldrPropio :: (a -> b -> b) -> b -> [a] -> b
foldrPropio _ inicial [] = inicial
foldrPropio f inicial (x:xs) = f x (foldrPropio f inicial xs)

foldlPropio :: (b -> a -> b) -> b -> [a] -> b
foldlPropio _ acumulador [] = acumulador
foldlPropio f acumulador (x:xs) = foldlPropio f (f acumulador x) xs
