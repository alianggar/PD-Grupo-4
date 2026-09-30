type Vector2D = (Double, Double)
type Posicion = (Double, Double)
type Tamaño = (Double, Double)
type Caja = (Posicion, Tamaño)
type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

-- Ejercicio 4: Utilidad de listas y cadenas
{-
Función: splitOn
Divide una cadena en trozos cada vez que aparece un carácter separador dado
> splitOn ',' "1,2,3"
["1","2","3"]
> splitOn ' ' "hola mundo cruel"
["hola","mundo","cruel"]
0.6 splitOn (recursión vía pattern matching x:xs; no usar Data.List.splitOn)
-}

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
-- con la constante r guarda el resultado de aplicar la funcion al resto de la cadena

{-
Función: trim
Elimina los espacios en blanco (espacios, tabuladores, saltos de línea) al principio y al final
de una cadena.
> trim " hola "
"hola"
> trim "\t sin espacios internos aqui \n"
"sin espacios internos aqui"
-}

trim :: String -> String

