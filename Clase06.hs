-- (1) lint: usar foldr
productoria :: [Int] -> Int
productoria [] = 1
productoria (x : xs) = x * productoria xs

-- (2) lint: usar map
sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (x + n) : sumarN n xs

-- (3)
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l = sumarN (head l) l

-- (4)
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (last l) l

-- (5)
pares :: [Int] -> [Int]
pares l
    | null l = []
    | even x = x : pares xs
    | otherwise = pares xs
    where
        (x:xs) = l

-- (6)
multiploDeN :: Int -> [Int] -> [Int]
multiploDeN n l
    | null l = []
    | mod x n == 0 = x : multiploDeN n xs
    | otherwise = multiploDeN n xs
    where
        (x:xs) = l

-- (7)
quitar' :: Int -> [Int] -> Bool -> [Int]
quitar' n l f
    | null l = []
    | not f && n == x = quitar' n xs True
    | otherwise = x : quitar' n xs f
    where
        (x:xs) = l

quitar :: Int -> [Int] -> [Int]
quitar n l = quitar' n l False

-- (8) lint: use foldr
estaEn :: Int -> [Int] -> Bool
estaEn n [] = False
estaEn n (x:xs) = n == x || estaEn n xs

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = estaEn x xs || hayRepetidos xs


-- (9)
eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos l
    | null l = []
    | x `estaEn` xs = eliminarRepetidos xs
    | otherwise = x : eliminarRepetidos xs 
    where (x:xs) = l

-- (10) lint: use foldl
maximo' :: Int -> [Int] -> Int
maximo' c [] = c
maximo' c (x:xs) = maximo' (max c x) xs

maximo :: [Int] -> Int
maximo (x:xs) = maximo' x xs

-- (11)
bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = min' : bubble (max' : xs)
    where
        min' = min x y
        max' = max x y

ordenar' :: Int -> [Int] -> [Int]
ordenar' 0 l = l
ordenar' n l = ordenar' (n - 1) (bubble l)

ordenar :: [Int] -> [Int]
ordenar l = ordenar' (length l) l

-- (12)
reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
