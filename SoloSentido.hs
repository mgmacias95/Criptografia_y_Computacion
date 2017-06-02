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
mochi_cifrado :: (Integral a, Random a) => [a] -> String -> [a]
mochi_cifrado s msg = f
    where
        b = binary_encoding msg
        t = length s
        z = (t - (length b `mod` t)) `mod` t
        c = b ++ replicate z 0
        d = chunksOf t c
        f = map (\x -> sum $ zipWith (\y z -> (fromIntegral y)*z) x s) d

merkle_hellman :: (Integral a, Random a) => a -> [a] -> [Int]
merkle_hellman a [] = []
merkle_hellman a (xs:s)
    | a >= xs   = 1:merkle_hellman (a-xs) s
    | otherwise = 0:merkle_hellman a s 

-- descifrado
mochi_descifrado :: (Integral a, Random a) => [a] -> a -> a -> [a] -> String
mochi_descifrado msg n u a = binary_decoding $ concat d
    where
        v   = inverse u n
        c   = map (\x -> x*v `mod` n) msg
        r   = reverse a
        d   = map (\x -> reverse $ merkle_hellman x r) c

{-
Ejercicio 2.
Sea $p$ un (pseudo-)primo mayor o igual que vuestro número de identidad. 
Encuentra un elemento primitivo $\alpha$, de $\mathbb{Z}_p^*$ 
(se puede usar el libro "_Handbook of Applied Cryptography_"); para facilitar 
el criterio, es bueno escoger $p$ de forma que $\frac{p - 1}{2}$ sea también 
primo, y para ell usamos Miller-Rabin). Definimos 

$$f:\mathbb{Z}_p \rightarrow \mathbb{Z}_p, x\mapsto\alpha^x$$


Calcula el inverso de tu fecha de nacimiento con el formato AAAAMMDD.
-}
is_primitive_root :: (Integral a, Random a) => a -> a -> Bool
is_primitive_root p a = exponential_zn a p2 p == (p-1)
    where
        p2 = (p - 1) `div` 2

find_primitive_root :: (Integral a, Random a) => a -> a
find_primitive_root p = head $ dropWhile (\x -> not $ is_primitive_root p x) primos
    where
        primos = filter (miller_rabin) [2..p-2]

inverso_nacimiento :: (Integral a, Random a) => a -> a -> a
inverso_nacimiento id birthday = baby_s_giant_s a birthday p
    where
        p = head $ dropWhile (\x -> not (miller_rabin x)) [id..id*2]
        a = find_primitive_root p

{-
En lo que sigue, p y q son enteros primos, y n = pq.

Ejercicio 3

Sea f:Z_n -> Z_n la función de Rabin: f(x) = x^2. 
Sea n = 48478872564493742276963. Sabemos que 
f(12) = 144= f(37659670402359614687722). Usando esta información, calcula $p$ 
y $q$ (mirar la demostración de "_Lecture Notes on Cryptography_", Lemma 2.43.
-}
get_pq :: (Integral a, Random a) => a -> a -> a -> (a,a)
get_pq n x y = (p,q)
    where
        (p,_,_) = extended_euclides (x-y) n
        q       = n `div` p

{-
Ejercicio 4

Elige a_0 y a_1 dos cuadrados arbitrarios módulo n (n como en el Ejercicio 3). 

Sea h: Z_2 x (Z_n)^*, h(b,x) = x^2a_0^ba_1^{1 - b}

Usa la función de Merkle-Damgard para implentar una función resumen tomando 
h como función de compresión (esta h fue definida por Glodwasser, Micali y Rivest). 
Los parámetros a_0, a_1 y n se hacen públicos (la función debería admitir un 
parámtero en el que venga especificado el vector inicial).
-}
gen_md_params :: (Integral a, Random a) => a -> String -> (a,a,[Int])
gen_md_params n s = (a0, a1, b)
    where
        b  = binary_encoding s
        l  = take 2 $ randomRs (1,n-1) $ mkStdGen (181876888)
        a0 = exponential_zn (head l) 2 n
        a1 = exponential_zn (last l) 2 n

merkle_damgard :: (Integral a, Random a) => (a, a, a) -> a -> [Int] -> a
merkle_damgard _ x []             = x
merkle_damgard (n,a0,a1) x (bs:b) = merkle_damgard (n,a1,a0) x' b
    where
        x2 = exponential_zn x 2 n
        b1 = 1 - bs
        x' = (x2 * (a0^bs) * (a1^b1)) `mod` n

{-
Ejercicio 5

Sea p el menor primo entero mayor o igual que tu número de identidad, y sea q el
primer primo mayor o igual que tu fecha de nacimiento (AAAAMMDD). Selecciona e tal
que gcd(e, (p-1)(q-1)) = 1. Define la función RSA

f: Z_n -> Z_n, x -> x^e

Calcula el inverso de 1234567890
-}

