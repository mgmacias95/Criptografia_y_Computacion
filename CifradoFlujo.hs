import Data.List
import Data.Bits

-- ternary operator
data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?
 
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

{-
Ejercicio 1.
Escribe una función que determine si una secuencia de bits cumple con los 
postulados de Golomb.
-}
golomb :: (Integral a) => [a] -> Bool
golomb s 
    | length s == 1 = True
    | otherwise     = cond1 && cond2 && cond3
        where
            s'      = head s == last s ? rotar s 1 :? s
            n_ones  = fromIntegral $ sum s
            n_zeros = (length s) - n_ones
            cond1   = abs (n_ones - n_zeros) <= 1
            b       = map (\x -> length x) (group s')
            n       = takeWhile (> 0) $ map (\x -> length $ elemIndices x b) [1..]
            n_c2    = dropWhile (\x -> fst x >= 2*snd x) (zip n (snd (splitAt 1 n))) 
            cond2   = length n_c2 <= 1
            dists   = map (\x -> hamming_distance_one s x) [1..length s-1]
            cond3   = (foldl1 (\ x y -> y - x) dists) == 0


rotar :: (Integral a) => [a] -> Int -> [a]
rotar s k = drop (length s - k) . take (2*(length s)-k) $ cycle s

hamming_distance_one :: (Integral a) => [a] -> Int -> a
hamming_distance_one s k = sum $ zipWith (\x y -> abs (x-y)) s (rotar s k)

{-
Ejercicio 2.
Implementa registros lineales de desplazamiento con retroalimentación (LFSR).
La entrada son los coeficientes del polinomio de conexión, la semilla y la 
longitud de la secuencia de salida.

Ilustra con ejemplos la dependencia del período de la semilla en el caso de
polinomios reducibles, la independencia en el caso de polinomios irreducibles
y la maximalidad en el caso de polinomios primitivos.

Comprueba que los ejemplos con polinomios primitivos satisfacen los postulados
de Golomb.
-}
lfsr :: (Integral a) => [Int] -> [Int] -> a -> [Int]
lfsr c s n
    | length c /= length s = error $ "La semilla y los coeficientes del" ++
                                     " polinomio deben tener el mismo tamaño"
    | otherwise            = s ++ lst
        where
            seq = take ((fromIntegral n) - (length c - 1)) $ iterate (\x -> drop 1 (x ++ 
                [mod (sum $ zipWith (.&.) c x) 2])) s
            lst = map last $ tail seq

{-
Ejercicio 3.

Escribe una función que toma como argumentos una  función polinómica $f$, 
una semilla $s$ y un entero positivo $k$, y devuelve una secuencia de longitud $k$ 
generada al aplicar a $s$ el registro no lineal de desplazamiento con 
retroalimentación asociado a $f$.

Encuentra el período de la NLFSR $((x \wedge y) \vee \bar{z}) \otimes t$ 
con semilla $1101$.
-}
func :: [Int] -> Int
func l
    | length l /= 4 = error "La semilla debe tener tamaño 4"
    | otherwise     = xor t $ (.|.) z $ (.&.) x y
        where
            x = l !! 0
            y = l !! 1
            z = ((l !! 2) - 1) `mod` 2
            t = l !! 3

nlfsr :: (Integral a) => ([Int] -> Int) -> [Int] -> a -> [Int]
nlfsr f s k = s ++ lst
    where
        seq = take ((fromIntegral k) - (length s -1)) $ iterate (\x -> drop 1 
            (x ++ [func x])) s
        lst = map last $ tail seq

{-
Ejercicio 4.
Implementa el generador de Geffe.

Encuentra ejemplos donde el periodo de la salida es p1p2p3, con p1, p2 y p3 los
períodos de los tres LFSRs usados en el generador de Geffe.

Usa este ejercicio para construir un cifrado en flujo. Con entrada un mensaje m,
construye una llave k con la misma longitud que m, y devuelve m xor k.

El descifrado se hace de la misma forma: c xor k.
-}
geffe :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
geffe p1 s1 p2 s2 p3 s3 = zipWith3 (\x y z -> (.|.) z $ (.|.) x y) x12 x23 p3'
    where
        l   = (length p3) * (length p1) * (length p2)
        p1' = lfsr p1 s1 l
        p2' = lfsr p2 s2 l
        p3' = lfsr p3 s3 l
        x12 = zipWith (.&.) p1' p2'
        x23 = zipWith (.&.) p2' p3'

{-
Ejercicio 5.
Dada una sucesión de bits periódica, determina la complejidad lineal de dicha
sucesión, y el polinomio de conexión que la genera. Para esto, usa el algoritmo de
Berlekamp-Massey.

Haz ejemplos con sumas y productos de secuencias para ver qué ocurre con 
la complejidad lineal
-}

b_massey :: (Integral a) => [Int] -> (a, [Int])
b_massey s = b_massey_aux (k+1) k 0 (k+1) f g s 
    where
        k = fromIntegral $ length $ takeWhile (/=1) s
        f = 1:replicate (fromIntegral k) 0 ++ 1:replicate (length s - (fromIntegral k) - 1) 0
        g = 1:replicate (length s - 1) 0

b_massey_aux :: (Integral a) => a -> a -> a -> a -> [Int] -> [Int] -> [Int] -> (a, [Int])
b_massey_aux l a b r f g s
    | fromIntegral r >= length s = ((fromIntegral l),f)
    | d `mod` 2 == 0             = b_massey_aux l a (b+1) (r+1) f g s
    | 2*l > r                    = b_massey_aux l a (b+1) (r+1) f' g s
    | otherwise                  = b_massey_aux (r-l+1) b b (r+1) f'' f s
        where
            d   = sum $ zipWith (.&.) (take (fromIntegral l + 1) f) (take (fromIntegral 
                l + 1) $ split_or_add_at (r-l) s)
            af  = zipWith (xor) (take (fromIntegral l + 1) f) 
                (take (fromIntegral l + 1) $ split_or_add_at (b-a) g)
            f'  = af ++ replicate (length s - length af) 0
            af' = zipWith (xor) (take (fromIntegral $ r+l+1) g) 
                (take (fromIntegral $ r+l) $ split_or_add_at (a-b) f)
            f'' = af' ++ replicate (length s - length af') 0

split_or_add_at :: (Integral a) => a -> [Int] -> [Int]
split_or_add_at n l
    | n == -1   = 0:l
    | n < -1    = replicate (abs $ fromIntegral $ n) 0 ++ l
    | otherwise = snd $ splitAt (fromIntegral n) l