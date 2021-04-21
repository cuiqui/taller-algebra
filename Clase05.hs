import Clase03(fib)

{-
    Calcular la suma de los divisores de un entero

        sumaDivisores 6 = 1 + 2 + 3 + 6 = 12
-}
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n m
    | m == 1 = 1
    | mod n m == 0 = m + sumaDivisoresHasta n (m - 1)
    | otherwise = sumaDivisoresHasta n (m - 1)

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n m
    | n == m = n
    | mod n m == 0 = m + sumaDivisoresDesde n (m + 1)
    | otherwise = sumaDivisoresDesde n (m + 1)

{-
    Ejercicios números primos
-}
menorDivisor' :: Int -> Int -> Int
menorDivisor' n m
    | mod n m == 0 = m
    | otherwise = menorDivisor' n (m + 1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisor' n 2

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n
    | esPrimo n = n
    | otherwise = minimoPrimoDesde (n + 1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n - 1))

{-
    Ejercicios factorial
-}
fact :: Integral p => p -> p
fact 0 = 1
fact n = n * fact (n - 1)

menorFactDesde :: Int -> Int
menorFactDesde = menorFactDesdeDesde 1

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m
    | fact i >= m = fact i
    | otherwise = menorFactDesdeDesde (i + 1) m

-- Ejercicios
-- (7)
mayorFactHasta' :: Integral p => p -> p -> p
mayorFactHasta' i m
    | fact i <= m = fact i
    | otherwise = mayorFactHasta' (i - 1) m

mayorFactHasta :: Integral p => p -> p
mayorFactHasta m = mayorFactHasta' m m

-- (8)
esFact' :: Int -> Int -> Bool
esFact' n i
    | fact i > n = False
    | fact i < n = esFact' n (i + 1)
    | otherwise = True

esFact :: Int -> Bool
esFact n = esFact' n 1

-- (9)
esFibonacci' :: Int -> Int -> Bool
esFibonacci' n i
    | fib i > n = False
    | fib i < n = esFibonacci' n (i + 1)
    | otherwise = True

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacci' n 1

-- (10)
sumaInicialDePrimos :: Int -> Int
sumaInicialDePrimos 0 = 0
sumaInicialDePrimos n = nEsimoPrimo n + sumaInicialDePrimos (n - 1)

esSumaInicialDePrimos' :: Int -> Int -> Bool
esSumaInicialDePrimos' n m = sumaInicialDePrimos m == n

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimos' n 20

-- (11)
maxSumaDeDivisores :: Int -> Int -> Int -> Int
maxSumaDeDivisores n1 n2 c
    | n1 > n2 = c
    | otherwise = maxSumaDeDivisores (n1 + 1) n2 biggest
    where
        biggest  = if sumaDivisores c >= sumaDivisores n1 then c else n1

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = maxSumaDeDivisores n1 n2 n1

-- (12)
minSumaDeDivisores :: Int -> Int -> Int -> Int
minSumaDeDivisores n1 n2 c
    | n1 > n2 = c
    | otherwise = minSumaDeDivisores (n1 + 1) n2 smallest
    where
        smallest  = if sumaDivisores c <= sumaDivisores n1 then c else n1

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = minSumaDeDivisores n1 n2 n1
