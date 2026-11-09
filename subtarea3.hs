type BoundingBox = (Float, Float, Float, Float)
type Vector2D=(Float,Float)
class Objeto a where
    posicion    :: a -> Vector2D
    boundingBox :: a -> BoundingBox
    animar      :: Float -> a -> a
    animar _ = id 


data Plataforma = Plataforma BoundingBox
    deriving (Eq, Show)

data Meta = Meta { posMeta :: Vector2D }
    deriving (Eq, Show)


instance Objeto Plataforma where
   
    posicion (Plataforma (x, y, _, _)) = (x, y)

    boundingBox (Plataforma bb) = bb


instance Objeto Meta where
    posicion m = posMeta m

    boundingBox (Meta (x, y)) = (x, y, 70, 80)


instance Objeto Jugador where
    posicion j = posJugador j
   
    boundingBox j = 
        let (x, y) = posJugador j 
        in (x - 30, y, 60, 90)


instance Objeto Enemigo where
    posicion e = posEnemigo e
    boundingBox e =
        let (x, y) = posEnemigo e
            (w, h) = dimEnemigo e
        in (x - w/2, y - h/2, w, h)
    animar dt e = 
        let desp = escalarVector dt (velEnemigo e)
            nuevaPos = sumaVectores (posEnemigo e) desp
        in e { posEnemigo = nuevaPos }


colisionaCon :: (Objeto a, Objeto b) => a -> b -> Bool
colisionaCon a b = solapan (boundingBox a) (boundingBox b)


colisionaConAlguno :: (Objeto a, Objeto b) => a -> [b] -> Bool
colisionaConAlguno a xs = any (colisionaCon a) xs
