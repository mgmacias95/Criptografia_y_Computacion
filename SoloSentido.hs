import Data.List
import Data.List.Split (chunksOf)
import System.Random (Random, randomRs, mkStdGen, randomR)
import AritmeticaModular
import CifradoFlujo (binary_encoding, binary_decoding)

{-
Ejercicio 1.
Sea (a1,...,ak) una secuencia super-creciente de números positivos (la suma de 
todos los términos que preceden a ai es menor que ai para todo i). Elige 
n > sum(ai) y u un entero positivo tal que gcd(n,u)=1. Define ai* = uai mod n.
La función mochila (knapsack) asociada a (a1*,...,ak*) es 

f: Z_2^k -> N, f(x1,...,xk) = sum_i=1 ^k xiai*.

Implementa esta función y su inversa. La llave pública es (a1*,...,ak*), mientras
que la privada (y la puerta de atrás) es ((a1,...ak),n,u).
-}
-- Generación de una secuencia super-creciente
genera_secuencia :: (Integral a, Random a) => a -> [a]
genera_secuencia t = take (fromIntegral t) $ iterate (\x -> x*2) r
    where
        r = fst $ randomR (2,20) $ mkStdGen (238012)

-- función que comprueba si dos números son primos relativos
is_prime_relative :: (Integral a) => a -> a -> Bool
is_prime_relative a b = x == 1
    where
        (x,_,_) = extended_euclides a b

-- función que genera tanto la llave pública como la privada
mochi_gen_claves :: (Integral a, Random a) => [a] -> ([a], a, a, [a])
mochi_gen_claves s = (a,n,u,s)
    where
        n  = (sum s) * 2
        u  = head $ dropWhile (\x -> not (is_prime_relative x n)) $ randomRs (1,n-1) 
             $ mkStdGen (28165137)
        a  = map (\x -> x*u `mod` n) s

-- encriptado
mochi_encriptado :: (Integral a, Random a) => [a] -> String -> [a]
mochi_encriptado s msg = f
    where
        b = binary_encoding msg
        t = length s
        z = (t - (length b `mod` t)) `mod` t
        c = b ++ replicate z 0
        d = chunksOf t c
        f = map (\x -> sum $ zipWith (\y z -> (fromIntegral y)*z) x s) d