--EJERCICIO 1 
{-Puntuación:
0.3 pts: Definición correcta del tipo sinónimo para un punto/vector 2D, usado de forma
consistente en las tres funciones
0.4 pts: sumaVectores
0.4 pts: escalarVector
0.4 pts: distancia-}

type Vector2D = (Float, Float)

--Función sumaVectores: suma componente a componente dos vectores/puntos 2D.

sumaVectores:: Vector2D -> Vector2D -> Vector2D
sumaVectores (a,b) (c,d) = (a + c, b + d)

--Función escalarVector: multiplica un vector/punto 2D por un factor escalar.

escalarVector:: Float -> Vector2D -> Vector2D
escalarVector n (a,b) = (n * a, n * b)

--Función distancia: calcula la distancia euclidea entre dos puntos 2D.

distancia:: Vector2D -> Vector2D -> Float
distancia (a,b) (c,d) = sqrt((c - a)**2 + (d - b)**2)

