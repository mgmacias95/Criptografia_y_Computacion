# Práctica 1 - Aritmética Modular

## Ejercicio 1

_Implementa el **Algoritmo extendido de Euclides** para el cálculo del máximo común divisor: dados dos enteros
![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png), encuentra ![equation](http://mathurl.com/h8ppwby.png) tales que ![equation](http://mathurl.com/gtyjyot.png) es el máximo común divisor de ![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png)._

Para resolver este ejercicio, he hecho dos funciones en _Haskell_: 

- `extended_euclides`: esta función sirve como interfaz para el usuario y comprueba el caso base, que ![equation](http://mathurl.com/zoohk97.png). En el caso de que no se de el caso base, se pasa a utilizar la siguiente función.

- `extended_euclides_tabla`: esta función implementa la tabla para el cálculo del algoritmo extendido de euclides. A diferencia de la anterior, necesita como entrada los valores de ![equation](http://mathurl.com/glwyhq6.png) además de ![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png).

## Ejercicio 2

_Usando el ejercicio anterior, escribe una función que calcule ![equation](http://mathurl.com/hhcnwnr.png) para cualesquiera ![equation](http://mathurl.com/jku6bzf.png) que sean primos relativos._

Para resolver este ejercicio, he calculado el ![equation](http://mathurl.com/26qs3uq.png) tal que ![equation](http://mathurl.com/gtyjyot.png). Esto es posible debido a que ![equation](http://mathurl.com/25elof5.png) y ![equation](http://mathurl.com/25js5ug.png) son primos relativos y su máximo común divisor es 1. Si no se da este caso, el programa devuelve un -1.

Una vez he obtenido ese valor, he calculado su valor modulo ![equation](http://mathurl.com/25js5ug.png). 


## Ejercicio 3

_Escribe una función que calcule ![equation](http://mathurl.com/hyy3kpk.png) para cualesquiera ![equation](http://mathurl.com/25elof5.png), ![equation](http://mathurl.com/25js5ug.png) y ![equation](http://mathurl.com/2wdwb4o.png) enteros positivos. La implementación debería tener en cuenta la representación binaria de ![equation](http://mathurl.com/25js5ug.png)._

Para hacer este ejercicio, podríamos primero realizar la potencia ![equation](http://mathurl.com/27tvygk.png) y después, calcular el módulo ![equation](http://mathurl.com/2wdwb4o.png) del resultado. Ahora bien, para ![equation](http://mathurl.com/27tvygk.png) muy grandes, esta aproximación es más que costosa.

Es por eso que debe considerarse la representación binaria de ![equation](http://mathurl.com/zbqqxv7.png). Usando dicha representación binaria, ![equation](http://mathurl.com/27tvygk.png) puede calcularse como:

![equation](http://mathurl.com/znlr2gc.png)

El algoritmo da tantos pasos como dígitos tenga la representación binaria de ![equation](http://mathurl.com/25js5ug.png) pero la complejidad de los cálculos está limitada por ![equation](http://mathurl.com/2wdwb4o.png) y no perderemos tiempo haciendo una potencia muy grande para después aplicarle un módulo.