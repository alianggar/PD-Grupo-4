{-  3 Lado de colisión (1.5 puntos)
Puntuación:
0.7 pts: Cálculo correcto de los 4 solapes parciales (usando where)
0.8 pts: Guardas que eligen el lado correcto en todos los ejemplos
Función: ladoColision
Dadas dos cajas que colisionan, devuelve el lado por el que se produce el contacto (el de
menor solape).-}

--type Posicion = (Double, Double)
--type Tamaño = (Double, Double)
--type Caja = (Posicion, Tamaño)

-- Caja (x,y,l,a)
-- La caja empieza en el punto (x,y)
-- y tiene largo l (eje x) y altura a (eje y)

-- Definición del tipo de dato para el resultado de la colisión
data Lado = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

-- Definición del tipo sinónimo para las cajas (x, y, largo, alto). 
type Caja = (Float, Float, Float, Float)

ladoColision :: Caja -> Caja -> Lado
-- Calcula el lado de colisión entre dos cajas (La primera es la que colisiona con la segunda)
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