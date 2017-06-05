module SoloSentido (mochi_gen_claves, mochi_cifrado, mochi_descifrado, 
                    inverso_nacimiento, get_pq, gen_md_params, merkle_damgard,
                    inverso_rsa, find_pq_rsa, dss_keys, firma_dss, 
                    check_firma_dss, rsa_keys, firma_rsa, check_firma_rsa) where
    
    import Data.List
    import Data.List.Split (chunksOf)
    import System.Random (Random, randomRs, mkStdGen, randomR)
    import AritmeticaModular
    import CifradoFlujo (binary_encoding, binary_decoding)
    import Crypto.Hash.SHA1 (hash)
    import Data.ByteString.Char8 (unpack, pack)
    import Data.ByteString.Base16 (encode)
    import Numeric (readInt)
    import Data.Char (isHexDigit, digitToInt)

    -- ternary operator
    data Cond a = a :? a
     
    infixl 0 ?
    infixl 1 :?
     
    (?) :: Bool -> Cond a -> a
    True  ? (x :? _) = x
    False ? (_ :? y) = y

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

    find_next_prime :: (Integral a, Random a) => a -> a
    find_next_prime p = head $ dropWhile (\x -> not (miller_rabin x)) [p..p*2]

    inverso_nacimiento :: (Integral a, Random a) => a -> a -> a
    inverso_nacimiento id birthday = baby_s_giant_s a birthday p
        where
            p = find_next_prime p
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
    merkle_damgard (n,a0,a1) x (bs:b) = merkle_damgard (n,a0,a1) x' b
        where
            b1 = 1 - bs
            x' = ((x^2) * (a0^bs) * (a1^b1)) `mod` n

    {-
    Ejercicio 5

    Sea p el menor primo entero mayor o igual que tu número de identidad, y sea q el
    primer primo mayor o igual que tu fecha de nacimiento (AAAAMMDD). Selecciona e tal
    que gcd(e, (p-1)(q-1)) = 1. Define la función RSA

    f: Z_n -> Z_n, x -> x^e

    Calcula el inverso de 1234567890
    -}
    inverso_rsa :: (Integral a, Random a) => a -> a -> a -> (a,a)
    inverso_rsa id birthday msg = (c, exponential_zn c e n)
        where
            p     = find_next_prime id
            q     = find_next_prime birthday
            phi_n = (p-1)*(q-1)
            e     = head $ dropWhile (\x -> not $ is_prime_relative x phi_n) [2..phi_n]
            d     = inverse e phi_n
            n     = p*q
            c     = exponential_zn msg d n

    {-
    Ejercicio 6

    Sea n=50000000385000000551, y sabemos que una inversa de Z_n -> Z_n, x -> x^5 es
    x -> x^10000000074000000101 (esto es, conoces tanto la llave pública como la 
    privada de la función RSA). Encuentra p y q usando el método explicado en "Notes
    on Cryptography", página 92. Compara este procedimiento con el algoritmo de 
    Miller-Rabin y el ejercicio 3.
    -}
    find_pq_rsa :: (Integral a, Random a) => a -> a -> a -> (a,a)
    find_pq_rsa n e d
        | g /= 1     = (g, n `div` g)
        | abs y == 1 = (0,0)
        | otherwise  = find_pq_rsa_aux n (exponential_zn y 2 n) y
            where
                b       = snd $ descomposicion_2us (d*e - 1) 0
                x       = fst $ randomR (0,n) $ mkStdGen (51518732)
                (g,_,_) = extended_euclides x n
                y       = exponential_zn x b n

    find_pq_rsa_aux :: (Integral a, Random a) => a -> a -> a -> (a,a)
    find_pq_rsa_aux n (-1) z = (0,0)
    find_pq_rsa_aux n 1 z    = (p,q)
        where
            (p,_,_) = extended_euclides (z+1) n
            (q,_,_) = extended_euclides (z-1) n
    find_pq_rsa_aux n y z    = find_pq_rsa_aux n (exponential_zn y 2 n) y 

    {-
    Ejercicio 7

    En este ejercicio se pide implementar un sistema de firma digital y verificación
    de la firma. Se puede elegir entre RSA o DSS.

    Al igual que antes, debe realizar tres tareas: generación de claves (ejercicios
    anteriores), generación de clave de firma y verificación de la firma.

    Para la generación de la firma, se le introducirá un mensaje a cifrar (Texto) y
    el fichero con la clave (privada), y deberá generar una firma, que se guardará en
    un fichero de texto.

    Puesto que lo que realmente se firma no es el mensaje, sino un resumen del 
    mensaje, hay que generar un resumen de dicho mensaje. Para esto emplearemos la
    función SHA1 (se pueden otras funciones resumen). Cualquiera de las 
    implementaciones de esta función que hay en la red puede ser usada.

    Para la verificación de la firma, se introduce el mensaje (fichero) que se ha
    firmado, un fichero con la firma (con el mismo formato que el apartado anterior)
    y un fichero con la clave (pública). Deberá responder si la firma es o no
    válida.
    -}
    sha1_hash :: (Integral a) => String -> a
    sha1_hash msg = fst $ head $ readInt 16 isHexDigit digitToInt $ unpack $
                    encode $ hash $ pack msg

    -- FIRMA DSS
    dss_keys :: (Integral a, Random a) => (a,a,a,a,a)
    dss_keys = (p,q,a,y,x)
        where
            q = head $ dropWhile (\x -> not $ miller_rabin x) $ 
                randomRs (2^5,2^6) $ mkStdGen (18777349)
            t = fst $ randomR (0::Integer,8) $ mkStdGen (78878965)
            p = head $ dropWhile (\x -> (not $ miller_rabin x) || (((x-1) `mod` q) /= 0)) 
                [2^(12+64*t)..2^(13+64*t)]
            g = head $ dropWhile (\x -> exponential_zn x ((p-1) `div` q) p == 1) $ 
                randomRs (2,p-1) $ mkStdGen (44451566)
            a = exponential_zn g ((p-1) `div` q) p
            x = fst $ randomR (2,q-2) $ mkStdGen (555556)
            y = exponential_zn a x p


    firma_dss :: (Integral a, Random a) => String -> a -> (a,a,a,a) -> (a,a)
    firma_dss m x (p,q,a,y) = (r,s)
        where
            h = sha1_hash m
            k = fst $ randomR (2, q-2) $ mkStdGen (1854877354)
            r = (exponential_zn a k p) `mod` q
            s = ((h + x*r) * (inverse k q)) `mod` q

    check_firma_dss :: (Integral a, Random a) => String -> (a, a) -> (a,a,a,a) -> Bool
    check_firma_dss m (r,s) (p,q,a,y)
        | r >= q || r <= 0 || s >= q || s <= 0 = False
        | otherwise                            = r == r'
            where
                h  = sha1_hash m
                i  = inverse s q
                u  = (h * i) `mod` q
                v  = (r * i) `mod` q
                r' = ((exponential_zn a u p) * (exponential_zn y v p) `mod` p) `mod` q 

    -- FIRMA RSA
    rsa_keys :: (Integral a, Random a) => (a,a,a)
    rsa_keys = (n,e,d)
        where
            p   = find_next_prime $ fst $ randomR (2^10,2^11) $ mkStdGen (666)
            q   = find_next_prime $ fst $ randomR (2^13,2^14) $ mkStdGen (1554)
            n   = p*q
            phi = (p-1)*(q-1)
            e   = head $ dropWhile (\x -> not $ is_prime_relative x phi) [2..phi]
            d   = inverse e phi

    firma_rsa :: (Integral a, Random a) => String -> a -> a -> a
    firma_rsa m d n = exponential_zn h d n
        where
            h = sha1_hash m

    check_firma_rsa :: (Integral a, Random a) => String -> (a,a) -> a -> Bool
    check_firma_rsa m (n,e) f = h `mod` n == exponential_zn f e n
        where
            h = sha1_hash m
