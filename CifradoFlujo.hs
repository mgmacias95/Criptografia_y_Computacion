import Data.List

{-
Ejercicio 1.
Escribe una función que determine si una secuencia de bits cumple con los 
postulados de Golomb.
-}
golomb :: (Integral a) => [a] -> Bool
golomb s = cond1 && cond2
    where
        n_ones  = fromIntegral $ sum s
        n_zeros = (length s) - n_ones
        cond1   = abs (n_ones - n_zeros) <= 1
        b       = map (\x -> length x) (group s)
        n       = takeWhile (> 0) $ map (\x -> length $ elemIndices x b) [1..]
        n_c2    = dropWhile (\x -> fst x >= 2*snd x) (zip n (snd (splitAt 1 n))) 
        cond2   = length n_c2 <= 1
