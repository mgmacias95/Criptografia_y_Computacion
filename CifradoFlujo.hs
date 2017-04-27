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
lfsr :: [Int] -> [Int] -> Int -> [Int]
lfsr c s n
    | length c /= length s = error $ "La semilla y los coeficientes del" ++
                                     " polinomio deben tener el mismo tamaño"
    | otherwise            = s ++ lst
        where
            seq = take (n - (length c - 1)) $ iterate (\x -> drop 1 (x ++ 
                [mod (sum $ zipWith (.&.) c x) 2])) s
            lst = map last $ tail seq
