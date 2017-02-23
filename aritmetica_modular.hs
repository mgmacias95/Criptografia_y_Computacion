{-
Ejercicio 1

Implementa el algoritmo extendido de Euclides para el cálculo del máximo común divisor: dados dos enteros
a y b, encuentra u, v \in Z tales que au + bv es el máximo común divisor de a y b
-}
extended_euclides :: Int -> Int -> (Int, Int, Int)
extended_euclides a 0 = (a, 1, 0)
extended_euclides a b = extended_euclides_tabla a b 1 0 0 1

-- Función auxiliar para el algoritmo extendido de euclides
extended_euclides_tabla :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
extended_euclides_tabla a b x2 x1 y2 y1 = extended_euclides_tabla b r x1 x y1 y
			where
				q = a `div` b
				r = a - q*b
				x = x2 - q*x1
				y = y2 - q*y1


{-
Ejercicio 2

Usando el ejercicio anterior, escribe una función que calcule a^{-1} mod b para cualesquiera a,b \in Z que
sean primos relativos.
-}
inverse :: Int -> Int -> Int
inverse a b = i `mod` b
	where
		i = extended_euclides a b !! 1
