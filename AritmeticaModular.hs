module AritmeticaModular (extended_euclides, inverse, exponential_zn, descomposicion_2us,
                          miller_rabin, baby_s_giant_s, cuadrados, raices_cuadradas, 
                          metodo_fermat, func, rho) where
    
    import System.Random -- instalar con cabal install random
    import System.IO.Unsafe
    import Data.List 
    import Data.Maybe
    import Data.Map (Map) -- This just imports the type name
    import qualified Data.Map as Map  -- Imports everything else, but with names 
                                      -- prefixed with "Map." (with the period).

    -- ternary operator
    data Cond a = a :? a
     
    infixl 0 ?
    infixl 1 :?
     
    (?) :: Bool -> Cond a -> a
    True  ? (x :? _) = x
    False ? (_ :? y) = y

    {-
    Ejercicio 1

    Implementa el algoritmo extendido de Euclides para el cálculo del máximo común divisor: 
    dados dos enteros'a y b, encuentra u, v \in Z tales que au + bv es el máximo común divisor 
    de a y b
    -}
    extended_euclides :: Integral a => a -> a -> (a, a, a)
    extended_euclides a 0 = (a, 1, 0)
    extended_euclides a b = extended_euclides_tabla a b 1 0 0 1

    -- Función auxiliar para el algoritmo extendido de euclides
    extended_euclides_tabla :: Integral a => a -> a -> a -> a -> a -> a -> (a, a, a)
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
    inverse :: Integral a => a -> a -> a
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
    exponential_zn :: Integral a => a -> a -> a -> a
    exponential_zn _ 0 _ = 1
    exponential_zn a 1 n = a `mod` n
    exponential_zn a k n = exponential_zn_aux a k n 1 ki
                    where
                        ki = k `mod` 2

    exponential_zn_aux :: Integral a => a -> a -> a -> a -> a -> a
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

    Dado un entero p, escribe una función para determinar si p es (problablemente) 
    primo usando el método de Miller-Rabin
    -}

    -- Descompone un número p en 2^u * s
    descomposicion_2us :: Integral a => a -> a -> (a, a)
    descomposicion_2us p t 
        | m == 0    = descomposicion_2us u s
        | otherwise = (t, p)
                    where
                        u = p `div` 2
                        s = t + 1
                        m = p `mod` 2
        

    -- realiza el test de miller rabin 10 veces
    miller_rabin :: (Integral a, Random a) => a -> Bool
    miller_rabin p = foldl1 (&&) (map miller_rabin_once (take 10 (repeat p))) 

    -- comprueba que p >= 5
    miller_rabin_once :: (Integral a, Random a) => a -> Bool
    miller_rabin_once p
        | even p && p > 2             = False
        | (odd p && p <= 5) || p == 2 = True
        | b == 1 || b == (p-1)        = True
        | otherwise                   = miller_rabin_ok p u b 0 0
                where
                    (u,s) = descomposicion_2us (p-1) 0
                    a     = unsafePerformIO (randomRIO (2, p-2))
                    b     = exponential_zn a s p

    -- realiza el test de miller rabin
    miller_rabin_ok :: (Integral a, Random a) => a -> a -> a -> a -> a -> Bool
    miller_rabin_ok p u a i b
        | a == 1 && b == (p-1) = True
        | a == 1 && b /= (p-1) = False
        | i >= u               = False
        | otherwise            = miller_rabin_ok p u c (i+1) a
            where
                c = exponential_zn a 2 p

    {-
    Ejercicio 5

    Implementa el algoritmo paso enano-paso gigante para el cálculo de logaritmos
    discretos en Zp.

    logaritmo en base 13 de 2 módulo 19 ---> baby_s_giant_s 13 2 19
    -}
    baby_step :: (Integral a, Random a) => a -> a -> a -> a -> Map a a
    baby_step a c p s = Map.fromList tabS
            where
                pa   = map (\x -> (exponential_zn a x p, x)) [0..(fromIntegral s)]
                tabS = map (\x -> ((fst x) * c `mod` p, (snd x))) pa

    giant_step :: (Integral a, Random a) => a -> a -> a -> a -> a -> Map a a -> a
    giant_step as t p count s tS
        | count == s      = -1
        | Map.member t tS = (count*s) - (tS Map.! t)
        | otherwise       = giant_step as x p (count+1) s tS
            where
                x = t * as `mod` p

    baby_s_giant_s :: (Integral a, Random a) => a -> a -> a -> a
    baby_s_giant_s a c p 
        | not $ miller_rabin p = error "p debe ser primo"
        | otherwise            = giant_step as as p 1 s tabS
            where
                s    = ceiling (sqrt (fromIntegral p-1))
                tabS = baby_step a c p s
                as   = exponential_zn a (fromIntegral s) p
    {-
    Ejercicio 6

    Sea n=pq, con p y q enteros y primos relativos
        * Escribe una función que, dado un entero a y un primo p con (a/p) = 1, devuelve r 
          tal que r^2=a mod p; primero te hará falta implementar el símbolo de Jacobi.

        * Sea a un entero que es residuo cuadrático módulo p y q. Usa el teorema chino de
          los restos para calcular todas las raíces cuadradas de a mod n a partir de las raíces
          cuadradas de a módulo p y q.
    -}
    -- simbolo de jacobi
    jacobi :: (Integral a) => a -> a -> a
    jacobi a n
        | even n    = error "n debe ser impar"
        | otherwise = jacobi_impar a n

    jacobi_impar :: (Integral a) => a -> a -> a
    jacobi_impar a n
        | a > n         = jacobi_impar (a `mod` n) n
        | a == 1        = 1
        | a == -1       = (-1)^((n - 1) `div` 2)
        | a == 2        = (-1)^((n^2 - 1) `div` 8)
        | impar && cond = -(jacobi_impar n a)
        | impar         = jacobi_impar n a
        | even a        = (jacobi_impar s n) * (jacobi_impar 2 n)^u
        | otherwise     = exponential_zn a ((n-1) `div` 2) n
                where
                    (u,s)     = descomposicion_2us a 0
                    impar     = odd a && odd n 
                    cond      = (a-3) `mod` 4 == 0 && (n-3) `mod` 4 == 0

    cuadrados :: (Integral a, Random a) => a -> a -> (a,a)
    cuadrados a p
        | not $ miller_rabin p = error "p debe ser primo"
        | jacobi a p /= 1      = error "(a/p) /= 1"
        | otherwise            = (raiz1, raiz2)
                where
                    n     = (fromIntegral $ fromJust $ elemIndex (-1) $ 
                            map (\x -> jacobi x p) [2..p-1]) + 2
                    (u,s) = descomposicion_2us (p-1) 0
                    b     = exponential_zn n s p
                    i     = inverse a p
                    raiz1 = cuadrados_ok a p n u s b i
                    raiz2 = p - raiz1


    cuadrados_aux :: (Integral a, Random a) => a -> a -> a -> a -> a -> [a] -> a
    cuadrados_aux _ _ r _ _ []     = r
    cuadrados_aux i b r u p (x:xs) = cuadrados_aux i (exponential_zn b 2 p) rb u p xs
            where
                r2 = exponential_zn r 2 p
                d  = exponential_zn (i*r2) (2^(u - 2 - x)) p
                rb = d == (p-1) ? (r*b `mod` p) :? r

    cuadrados_ok :: (Integral a, Random a) => a -> a -> a -> a -> a -> a -> a -> a
    cuadrados_ok a p _ 1 _ _ _ = a^((p+1) `div` 4)
    cuadrados_ok a p n u s b i = rlist
            where
                r     = exponential_zn a ((s+1) `div` 2) p
                r2    = exponential_zn r 2 p
                rlist = cuadrados_aux i b r u p [0..u-2]

    teorema_chino_resto :: (Integral a, Random a) => a -> a -> a -> a -> a
    teorema_chino_resto a1 a2 p q = (a1 + p*l) `mod` (p*q)
            where
                pi = inverse p q
                l  = (a2 - a1) * pi

    raices_cuadradas :: (Integral a, Random a) => a -> a -> a -> (a,a,a,a)
    raices_cuadradas r p q = (r1, r2, r3, r4)
            where
                (a1, a3) = cuadrados r p
                (a2, a4) = cuadrados r q
                n        = p*q
                r1       = teorema_chino_resto a1 a2 p q
                r2       = n - r1
                r3       = teorema_chino_resto a1 a4 p q
                r4       = n - r3

    {-
    Ejercicio 7

    * Implementa el método de Fermat para factorización de enteros
    * Implementa el algoritmo de factorización de Pollard
    -}

    -- Método de Fermat
    -- http://stackoverflow.com/questions/16228542/haskell-function-to-test-if-int-is-perfect-square-using-infinite-list#16228629
    isSquare n = elem n (takeWhile (<=n) [ x*x | x <- [1..]])

    metodo_fermat :: (Integral a, Random a) => a -> (a,a)
    metodo_fermat n = (x'+y', x'-y')
            where
                x       = ceiling (sqrt (fromIntegral n))
                c       = x^2 - n
                (x',y') = metodo_fermat_aux x c n

    metodo_fermat_aux :: (Integral a, Random a) => a -> a -> a -> (a,a)
    metodo_fermat_aux x c n
        | isSquare c = (x, floor (sqrt (fromIntegral c)))
        | otherwise  = metodo_fermat_aux x' c' n
                where
                    x' = x+1
                    c' = x'^2 - n

    -- Algoritmo rho de Pollard
    func :: (Integral a, Random a) => a -> a -> a
    func x n = (x^2 + c) `mod` n
        where
            r = unsafePerformIO $ randomIO
            c = r == 0 || r == -2 ? r+4 :? r

    rho :: (Integral a, Random a) => a -> (a -> a -> a) -> (a, a)
    rho n f 
        | r == n   = error "No se ha encontrado factor. Vuelve a lanzar la función."
        |otherwise = (r, n `div` r)
                where
                    x  = unsafePerformIO (randomRIO (0, n-1))
                    x' = f x n 
                    y  = f x' n
                    r  = rho_aux n f x' y

    rho_aux :: (Integral a, Random a) => a -> (a -> a -> a) -> a -> a -> a
    rho_aux n f x y
        | d /= 1    = d
        | otherwise = rho_aux n f x' y'
            where
                (d,_,_) = extended_euclides (y-x) n
                x'      = f x n
                y'      = f x' n
