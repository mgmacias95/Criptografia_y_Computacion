import System.Random -- instalar con cabal install random
import System.IO.Unsafe

{-
Ejercicio 1

Implementa el algoritmo extendido de Euclides para el cálculo del máximo común divisor: 
dados dos enteros'a y b, encuentra u, v \in Z tales que au + bv es el máximo común divisor 
de a y b
-}
extended_euclides :: Int -> Int -> (Int, Int, Int)
extended_euclides a 0 = (a, 1, 0)
extended_euclides a b = extended_euclides_tabla a b 1 0 0 1

-- Función auxiliar para el algoritmo extendido de euclides
extended_euclides_tabla :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
extended_euclides_tabla a 0 x2 _ y2 _ = (a, x2, y2)
extended_euclides_tabla a b x2 x1 y2 y1 = extended_euclides_tabla b r x1 x y1 y
            where
                q = a `div` b
                r = a - q*b
                x = x2 - q*x1
                y = y2 - q*y1


{-
Ejercicio 2

Usando el ejercicio anterior, escribe una función que calcule a^-1 mod b para cualesquiera 
a,b \in Z que sean primos relativos.
-}
inverse :: Int -> Int -> Int
inverse a b
    | r == 1 = i `mod` b
    | otherwise = -1
        where
            (r,i,_) = extended_euclides a b

{-
Ejercicio 3

Escribe una función que calcule a^b mod n para cualquiera a, b y n enteros positivos. 
La implementación debería tener en cuenta la representación binaria de b.
-}
exponential_zn :: Int -> Int -> Int -> Int
exponential_zn _ 0 _ = 1
exponential_zn a 1 _ = a
exponential_zn a k n = exponential_zn_aux a k n 1 ki
                where
                    ki = k `mod` 2

exponential_zn_aux :: Int -> Int -> Int -> Int -> Int -> Int
exponential_zn_aux a 1 n b _ = a*b `mod` n
exponential_zn_aux a k n b ki
    | ki == 0   = exponential_zn_aux a0 k0 n b ki0
    | otherwise = exponential_zn_aux a0 k0 n b0 ki0
                where
                    a0  = a*a `mod` n
                    k0  = k `div` 2
                    ki0 = k0 `mod` 2
                    b0  = a*b `mod` n

{-
Ejercicio 4

Dado un entero p, escribe una función para determinar si p es (problablemente) primo
usando el método de Miller-Rabin
-}

-- Descompone un número p en 2^u * s
descomposicion_2us :: Int -> Int -> (Int, Int)
descomposicion_2us p t 
    | m == 0    = descomposicion_2us u s
    | otherwise = (t, p)
                where
                    u = p `div` 2
                    s = t + 1
                    m = p `mod` 2

-- get_random_int :: (Int, Int) -> IO Int
-- get_random_int (min, max) = do
--     g <- newStdGen
--     let (n,_) = randomR (min, max) g :: (Int, StdGen)
--     return n
    

-- comprueba que p >= 5
miller_rabin :: Int -> Bool
miller_rabin p
    | p < 5     = error "Imposible aplicar test para p < 5"
    | otherwise = miller_rabin_ok p u s a
            where
                (u,s) = descomposicion_2us p 0
                a     = unsafePerformIO (randomRIO (2, p-2))

-- realiza el test de miller rabin
miller_rabin_ok :: Int -> Int -> Int -> Int -> Bool
miller_rabin_ok p u s a
        | b == 1 || b == p-1 = True
        | otherwise          = False
                where
                    b = exponential_zn a s p
