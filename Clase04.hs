import Clase03(eqDigitos)

-- ejercicios
-- (4)
{-
    Ejemplos:
        g1 2 4 = 2^2 + 2^(n-1) + 2^n = 4 + 8 + 16 = 28
        g1 0 3 = 0^0 + 0^1 + 0^2 + 0^3 = 1
        g1 1 2 = 1^1 + 1^2 = 1 + 1 = 2
-}
g1 :: Float -> Int -> Float
g1 i n
    | i == fromIntegral n = i ^ n
    | otherwise = i ^ n + g1 i (n - 1)
 
-- (5)
{-
    Ejemplos: 

    * g2 n
        1^1         + 1^2 + ... + 1^n +
        2^2         + 2^3 + ... + 2^n +
        3^3         + 3^4 + ... + 3^n +
        (n-1)^(n-1) + (n-1)^n +
        n^n

    * g2 2
        1^1 + 1^2 +
        2^2 = 4 + 1 + 1 = 6

    * g2 3
        1^1 + 1^2 + 1^3 +
        2^2 + 2^3 +
        3^3 = 27 + 8 + 4 + 1 + 1 + 1 = 42
-}
g2 :: Int -> Float
g2 0 = 0
g2 n = g1 1 n + g2 (n - 1)

-- (6)
{-
    * g3 3:
        2^1 + 2^2 + 2^3,
            pero 1 y 3 son impares,
            con lo cual: 2^2 = 4
    * g3 5
        2^1 + 2^2 + 2^3 + 2^4 + 2^5 => 2^2 + 2^4 = 4 + 16 = 20
-}
g3 :: Int -> Int
g3 n
    | n == 0 = 0
    | even n = 2 ^ n + g3 (n - 1)
    | otherwise = g3 (n - 1)

-- (7)
{-
    * g4 12
    1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 11 = 56
-}
g4 :: Int -> Int
g4 n
    | n == 0 = 0
    | eqDigitos n = n + g4 (n - 1)
    | otherwise = g4 (n - 1)
