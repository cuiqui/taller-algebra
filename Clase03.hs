{-
    * Int: usa la aritmética entera del procesador.
    * Integer: usa la aritmética entera emulada en software, puede representar
               (simular) números más grandes pero es más lento.

    Si usara la signatura

        factorial :: Int -> Int
    
    entonces

        λ > factorial 25
        7034535277573963776
        λ > factorial 26
        -1569523520172457984
    
    dado que el número excedería a la representación binaria de un entero
    en 32 bits.
-}

module Clase03
where
    factorial :: Integer -> Integer
    factorial n
        | n == 0 = 1
        | n > 0 = n * factorial (n - 1)

    factorialPM :: Int -> Int
    factorialPM 0 = 1
    factorialPM n = n * factorialPM (n - 1)

    -- dos casos base
    esPar :: Int -> Bool
    esPar n
        | n == 0 = True
        | n == 1 = False
        | otherwise = esPar (n - 2)

    -- un caso base
    esPar2 :: Int -> Bool
    esPar2 n
        | n == 0 = True
        | otherwise = not (esPar2 (n - 1))

    fib :: Int -> Int
    fib n
        | n == 0 = 0
        | n == 1 = 1
        | otherwise = fib (n - 2) + fib (n - 1)

    fibPM :: Int -> Int
    fibPM 0 = 0
    fibPM 1 = 1
    fibPM n = fibPM (n - 2) + fibPM (n - 1)

    {-
        Acá hay floating point error:
            λ > parteEntera 3.999999
            3
            λ > parteEntera 3.9999999
            4
    -}
    parteEntera :: Float -> Int
    parteEntera n
        | n < 1 = 0
        | otherwise = 1 + parteEntera (n - 1)

    -- ejercicios
    -- (1)
    multiplo3 :: Int -> Bool
    multiplo3 n
        | n == 3 = True
        | n < 3 = False
        | otherwise = multiplo3 (n - 3)

    -- (2)
    sumaImpares :: Int -> Int
    sumaImpares n
        | n == 1 = 1
        | otherwise = 2 * n - 1 + sumaImpares (n - 1)

    sumaImparesPM :: Int -> Int
    sumaImparesPM 1 = 1
    sumaImparesPM n = 2 * n - 1 + sumaImparesPM (n - 1)

    -- (3)
    medioFact :: Int -> Int
    medioFact n
        | n <= 1 = 1
        | otherwise = n * medioFact (n - 2)

    medioFactPM :: Int -> Int
    medioFactPM 0 = 1
    medioFactPM 1 = 1
    medioFactPM n = n * medioFactPM (n - 2)

    -- (4)
    sumaDigitos :: Int -> Int
    sumaDigitos n
        | n < 10 = n
        | otherwise = resto + sumaDigitos cociente
        where
            resto = mod n 10
            cociente = div n 10

    -- (5)
    eqDigitos :: Int -> Bool
    eqDigitos n
        | n < 10 = True
        | otherwise = (last == beforeLast) && eqDigitos rest
        where
            last = mod n 10
            beforeLast = div (mod n 100) 10
            rest = div n 10
