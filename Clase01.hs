doble :: Float -> Float
doble x = 2 * x

suma :: Float -> Float -> Float
suma x y = x + y

normaVectorial :: Float -> Float -> Float
normaVectorial x1 x2 = sqrt (x1^2 + x2^2)

funcionConstante8 :: Int -> Int
funcionConstante8 x = 8

f2 :: Int -> Bool
f2 n | n == 0 = True
     | otherwise = False

maximo :: Int -> Int -> Int
maximo x y | x >= y = x
           | otherwise = y

maximoRac :: Float -> Float -> Float
maximoRac x y | x >= y = x
              | otherwise = y

cantidadDeSoluciones :: Int -> Int -> Int
cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b^2 - 4*c

esPar :: Int -> Bool
esPar = even

esImpar :: Int -> Bool
esImpar n = not (esPar n)

-- Ejercicios
-- (1)
absoluto :: Int -> Int
absoluto n = maximo n (-n)

-- (2)
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y = maximo (absoluto x) (absoluto y)

-- (3)
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo (maximo x y) z

-- (4.i)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x == 0 || y == 0

-- (4.ii)
algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM _ 0 = True
algunoEs0PM 0 _ = True
algunoEs0PM _ _ = False

-- (5.i)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x == 0 && y == 0

-- (5.ii)
ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

-- (6)
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x == 0

-- (7)
digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

-- (8)
digitoDecenas :: Int -> Int
digitoDecenas n = div (mod n 100) 10
