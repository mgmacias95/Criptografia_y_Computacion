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

He hecho dos funciones _Haskell_ para este ejercicio:

* `exponential_zn`: que recibe como parámetros tres valores ![equation](http://mathurl.com/25elof5.png), ![equation](http://mathurl.com/25js5ug.png) y ![equation](http://mathurl.com/2wdwb4o.png) y comprueba los casos base (que ![equation](http://mathurl.com/zoohk97.png) o que ![equation](http://mathurl.com/gven6k9.png)).

* `exponential_zn_aux`: que realiza el cálculo de la potencia usando la representación binaria de ![equation](http://mathurl.com/25js5ug.png). Es importante destacar que no calculo la representación binaria de ![equation](http://mathurl.com/25js5ug.png) sino que voy calculando en cada paso el ![equation](http://mathurl.com/39x8zcn.png) correspondiente.

## Ejercicio 4

_Dado un entero ![equation](http://mathurl.com/3xbx475.png), escribe una función para determinar si ![equation](http://mathurl.com/3xbx475.png) es (problablemente) primo usando el método de **Miller-Rabin**._

El método de __Miller-Rabin__ tiene los siguientes pasos:

1. En primer lugar, debemos descomponer ![equation](http://mathurl.com/h85ahjb.png). Para ello, dividimos ![equation](http://mathurl.com/hm7h2e8.png) entre 2 hasta obtener resto 1. ![equation](http://mathurl.com/26qs3uq.png) es el número de divisiones que hemos hecho hasta obtener una con resto 1 (sin contar esta última) y ![equation](http://mathurl.com/yemlmqa.png) es el cociente de la última división con resto 0 que hemos hecho y debe ser __impar__. Este paso se realiza en la función `descomposicion_2us`.

2. Tomamos un ![equation](http://mathurl.com/25elof5.png) aleatorio tal que ![equation](http://mathurl.com/hm7zezn.png). En este paso, rompo la _transparencia referencial_ de Haskell, ya que cada vez que ejecuto la función se obtiene un ![equation](http://mathurl.com/25elof5.png) distinto:

```haskell
a = unsafePerformIO (randomRIO (2, p-2))
```

3. Calculamos el valor de ![equation](http://mathurl.com/jrraqse.png). Si es igual a 1 o a ![equation](http://mathurl.com/hm7h2e8.png), ![equation](http://mathurl.com/3xbx475.png) será probablemente primo.

4. Si no ha sido así, iteramos desde 1 hasta ![equation](http://mathurl.com/yemlmqa.png) elevando en cada paso ![equation](http://mathurl.com/jrraqse.png) al cuadrado. Si en algún paso obtenemos un 1, debemos comprobar que en el paso anterior obtuvimos un ![equation](http://mathurl.com/hm7h2e8.png). Si esto se cumple, ![equation](http://mathurl.com/3xbx475.png) será probablemente primo y si no, no será primo.

Todas las operaciones deben hacerse módulo ![equation](http://mathurl.com/3xbx475.png). Además, al ser un test probabilístico, debe realizarse un mínimo de veces para asegurar una baja probabilidad de error. En nuestro caso, al hacerlo 10 veces, la probabilidad de error es:

![equation](http://mathurl.com/jaxtzlm.png)
