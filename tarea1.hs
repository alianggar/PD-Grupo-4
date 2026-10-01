type Vector2D = (Double, Double)
type Posicion = (Double, Double)
type Tamaño = (Double, Double)
type Caja = (Posicion, Tamaño)
type Celda = Char
type Fila = [Celda]
type Grid = [Fila]

{-
Ejercicio 4: Utilidad de listas y cadenas
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
trim cadena = trimFinal (trimPrincipio cadena)
    where
    -- creamos una funcion para saber si un caracter es un espacio en blanco
        espacio :: Char -> Bool
        espacio cadena = cadena == ' ' || cadena == '\t' || cadena == '\n'
    -- ahora otra funcion para eliminar los espacios al principio de la cadena
        trimPrincipio :: String -> String
    -- primero el caso base, cuando es vacio devuelve la cadena vacia
        trimPrincipio "" = ""
        trimPrincipio (x:xs)
            | espacio x = trimPrincipio xs
        -- si el primer caracter es un espacio en blanco, devuelve el resto de la cadena
            | otherwise = x:xs
        -- en caso contrario, devuelve la cadena completa
    -- y por ultimo, otra funcion para eliminar los espacios al final de la cadena
        trimFinal :: String -> String
        trimFinal f = reverse (trimPrincipio (reverse f))
    -- le da la vuelta a la cadena y llama a la funcion trimPrincipio para eliminar si encuentra
    -- un espacio en blanco y vuelve a darle la vuelta a la cadena para dejarla en el orden correcto

{-
Función: contarSiCumple
Cuenta cuántos elementos de una lista cumplen una condición dada.
> contarSiCumple even [1,2,3,4,5,6]
3
> contarSiCumple (> 10) [1,20,3,40]
2
-}

contarSiCumple :: (a -> Bool) -> [a] -> Int 
contarSiCumple condicion xs = length [ x | x <- xs, condicion x]
-- recorremos todos los elementos de la lista, le aplicamos la condicion de la guarda para quedarnos solo
-- con las que lo cumplan y contamos los elementos que pasen el filtro de la condicion 

{-
Función: list2Vector2
Convierte una lista de dos (o más) números en un vector/punto 2D; lanza un error si la lista
no tiene al menos dos elementos.
> list2Vector2 [3.0, 4.0]
(3.0,4.0)
> list2Vector2 [3.0]
*** Exception: Falta un elemento en la lista para convertir en Vector2
> list2Vector2 []
*** Exception: Lista vacia no posible convertir en Vector2
-}

list2Vector2 :: [Double] -> Vector2D
-- caso de lista vacia
list2Vector2 [] = error "Lista vacia no posible convertir en Vector2"
-- caso lista solo con un elemento
list2Vector2 [x] = error "Falta un elemento en la lista para convertir en Vector2"
-- caso general con al menos 2 elementol, usamos el patron anonimo porque el resto de la lista no nos importa
list2Vector2 (x:y:_) = (x,y)

{-
Ejercicio 5
Función: parsearNivel
Convierte la lista de líneas de texto leídas de un fichero de nivel en la estructura de datos
que representa el nivel completo.
> parsearNivel ["..M", "#.X", "###"]
["..M","#.X","###"]
-}

parsearNivel :: [Fila] -> Grid
parsearNivel filas = filas 

{-
Función: esSolido
Indica si una celda es una plataforma sólida.
> esSolido '#'
True
> esSolido '.'
False
-}

esSolido :: Celda -> Bool
esSolido '#' = True
esSolido _ = False
-- uso de pattern matching para especificar que '#' es solido y cualquier otro, no lo es

{-
Función: esMeta
Indica si una celda es la meta del nivel.
> esMeta 'M'
True
> esMeta '#'
False
-}

esMeta :: Celda -> Bool
esMeta 'M' = True
esMeta _ = False

{-
Función: esVacio
Indica si una celda está vacía (no hay nada en ella).
> esVacio '.'
True
> esVacio 'M'
False
-}

esVacio :: Celda -> Bool
esVacio '.' = True
esVacio _ = False
-- lo mismo con es meta y es vacio

{-
Función: esEnemigo
Indica si una celda marca el punto de inicio de un enemigo (cualquier carácter que no sea
sólido, meta ni vacío).
> esEnemigo 'X'
True
> esEnemigo '#'
False
> esEnemigo '.'
False
-}

esEnemigo :: Celda -> Bool
esEnemigo c = not (esSolido c || esMeta c || esVacio c)

{-
Función: posicionesMeta
Dado el nivel completo, devuelve la lista de posiciones (fila, columna) en las que aparece la
meta.
> posicionesMeta ["..M", "#.X", "###"]
[(0,2)]
> posicionesMeta ["....", "...."]
[]
-}

