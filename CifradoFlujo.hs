import Data.List

{-
Ejercicio 1.
Escribe una función que determine si una secuencia de bits cumple con los 
postulados de Golomb.
-}
golomb :: (Integral a) => [a] -> Bool
golomb s = cond1
    where
        n_ones  = fromIntegral $ sum s
        n_zeros = (length s) - n_ones
        cond1   = abs (n_ones - n_zeros) <= 1

