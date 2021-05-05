type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece x [] = False
pertenece x (c:cs) = x == c || pertenece x cs

agregar :: Int -> Set Int -> Set Int
agregar x c
    | pertenece x c = c
    | otherwise = x:c

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss
    | xs `perteneceC` xss = xss
    | otherwise = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

incluido :: Set Int -> Set Int -> Bool
incluido [] b = True
incluido (a:as) b = pertenece a b && incluido as b

iguales :: Set Int -> Set Int -> Bool
iguales a b = incluido a b && incluido b a

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] b = b
unionC (a:as) b = agregarC a (unionC as b)

-- Ejercicios (I)
union :: Set Int -> Set Int -> Set Int
union [] b = b
union (a:as) b = agregar a (union as b)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] b = []
interseccion (a:as) b
    | pertenece a b = agregar a (interseccion as b)
    | otherwise = interseccion as b

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] b = []
diferencia (a:as) b
    | pertenece a b = diferencia as b
    | otherwise = agregar a (diferencia as b)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = diferencia a b `union` diferencia b a

-- Ejercicios (II)
partesN :: Int -> Set (Set Int)
partesN n = partes [1..n]

pc :: Int -> Set Int -> Set (Int, Int)
pc n [] = []
pc n (b:bs) = (n, b) : pc n bs

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] b = []
productoCartesiano (a:as) b = pc a b ++ productoCartesiano as b
