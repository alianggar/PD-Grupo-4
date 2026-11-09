-- Tamaño fijo de cada celda en el mundo jugable
tamCelda :: Float
tamCelda = 40.0

-- Calcula la esquina inferior izquierda de la celda (x, y) donde Y crece hacia arriba
posicionCelda :: Int -> Int -> Int -> Vector2
posicionCelda total i j =
  let x = fromIntegral j * tamCelda
      y = fromIntegral (total - 1 - i) * tamCelda
  in (x, y)

-- Genera una plataforma por cada celda 'Solido' del Grid mediante listas por comprensión
gridAPlataformas :: Grid -> [Plataforma]
gridAPlataformas grid =
  let total = length grid
  in [ Plataforma (x, y, tamCelda, tamCelda)
     | (i, fila) <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esSolido celda
     , let (x, y) = posicionCelda total i j
     ]

-- Construye los objetos Meta desde el Grid usando la función esMeta
metasDesdeGrid :: Grid -> [Meta]
metasDesdeGrid grid =
  let total = length grid
  in [ Meta (posicionCelda total i j)
     | (i, fila) <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , esMeta celda
     ]

-- Construye los objetos Enemigo filtrando las celdas InicioEnemigo
enemigosDesdeGrid :: Grid -> [Enemigo]
enemigosDesdeGrid grid =
  let total = length grid
  in [ Enemigo idEnemigo (posicionCelda total i j) (-40, 0) (tamCelda, tamCelda) Patrulla
     | (i, fila) <- zip [0..] grid
     , (j, celda) <- zip [0..] fila
     , InicioEnemigo idEnemigo <- [celda] -- Pattern matching en la lista por comprensión
     ]