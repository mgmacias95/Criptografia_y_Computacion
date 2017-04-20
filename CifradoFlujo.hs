import Data.List

{-
Ejercicio 1.
Escribe una función que determine si una secuencia de bits cumple con los 
postulados de Golomb.
-}
golomb :: (Integral a) => [a] -> Bool
golomb s = cond1 && cond2 && cond3
    where
        n_ones  = fromIntegral $ sum s
        n_zeros = (length s) - n_ones
        cond1   = abs (n_ones - n_zeros) <= 1
        b       = map (\x -> length x) (group s)
        n       = takeWhile (> 0) $ map (\x -> length $ elemIndices x b) [1..]
        n_c2    = dropWhile (\x -> fst x >= 2*snd x) (zip n (snd (splitAt 1 n))) 
        cond2   = length n_c2 <= 1
        dists   = map (\x -> hamming_distance_one s x) [1..length s-1]
        cond3   = (foldl1 (\ x y -> y - x) dists) == 0


rotate :: (Integral a) => [a] -> Int -> [a]
rotate s k = drop (length s - k) . take (2*(length s)-k) $ cycle s

hamming_distance_one :: (Integral a) => [a] -> Int -> a
hamming_distance_one s k = sum $ zipWith (\x y -> abs (x-y)) s (rotate s k)
