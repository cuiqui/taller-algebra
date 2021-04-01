-- ejercicios
-- (1)
estanRelacionados :: (Ord a, Num a) => a -> a -> Bool
estanRelacionados x y
    | x <= 3 && y <= 3 = True
    | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
    | x > 7 && y > 7 = True
    | otherwise = False
 
-- (2)
prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- (3)
todoMenor :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

-- (4)
distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos (x1, y1) (x2, y2) = sqrt (dx ^ 2 + dy ^ 2)
    where
        dx = x2 - x1
        dy = y2 - y1

-- (5)
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

-- (6)
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z)
    | even x = 1
    | even y = 2
    | even z = 3
    | otherwise = 4

-- (7)
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

-- (8)
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)
