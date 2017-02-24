# Práctica 1 - Aritmética Modular

## Ejercicio 1

_Implementa el **Algoritmo extendido de Euclides** para el cálculo del máximo común divisor: dados dos enteros
![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png), encuentra ![equation](http://mathurl.com/h8ppwby.png) tales que ![equation](http://mathurl.com/gtyjyot.png) es el máximo común divisor de ![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png)._

Para resolver este ejercicio, he hecho dos funciones en _Haskell_: 

- `extended_euclides`: esta función sirve como interfaz para el usuario y comprueba el caso base, que ![equation](http://mathurl.com/zoohk97.png). En el caso de que no se de el caso base, se pasa a utilizar la siguiente función.

- `extended_euclides_tabla`: esta función implementa la tabla para el cálculo del algoritmo extendido de euclides. A diferencia de la anterior, necesita como entrada los valores de ![equation](http://mathurl.com/glwyhq6.png) además de ![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png).

## Ejercicio 2

_Usando el ejercicio anterior, escribe una función que calcule ![equation](http://mathurl.com/hhcnwnr.png) para cualesquiera ![equation](http://mathurl.com/jku6bzf.png) que sean primos relativos._

Para resolver este ejercicio, he calculado el ![equation](http://mathurl.com/26qs3uq.png) tal que ![equation](http://mathurl.com/gtyjyot.png). Una vez he obtenido ese valor, he calculado su valor modulo ![equation](http://mathurl.com/25js5ug.png).
