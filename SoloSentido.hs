import Data.List
import System.Random -- instalar con cabal install random
import System.IO.Unsafe
import AritmeticaModular

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
genera_secuencia :: (Integral a, Random a) => a -> [a]
genera_secuencia t = take (fromIntegral t) $ iterate (\x -> x*2) r
    where
        r = unsafePerformIO $ randomRIO (5,20)

is_prime_relative :: (Integral a) => a -> a -> Bool
is_prime_relative a b = x == 1
    where
        (x,_,_) = extended_euclides a b

mochi_gen_claves :: (Integral a, Random a) => [a] -> (a, a)
mochi_gen_claves s = (m,u)
    where
        m = (sum s) * 2
        u = last $ take (m `div` 3) $ filter (\x -> is_prime_relative x m) [(m `div` 2)..m]
