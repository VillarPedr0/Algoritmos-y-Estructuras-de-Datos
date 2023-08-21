-- 1. Programá las siguientes funciones:
--    a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero x = x == 0
{-
ghci> esCero 3
False
ghci> esCero 0
True
-}
--    b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo x = x > 0
{-
ghci> esPositivo 4
True
ghci> esPositivo (-4)
False
-}
--    c ) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula.
esVocal :: Char -> Bool
esVocal x = x `elem` "aeiou"
{-
ghci> esVocal 'a'
True
ghci> esVocal 'v'
False
-}

-- 2. Programa las siguientes funciones usando recursion o composicion:
--    a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista
--    sean True.
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = (x == True) && paraTodo xs
{-
ghci> paraTodo [True, False]
False
ghci> paraTodo [True, True]
True
-}
-- ____________________________________________________________________________________________ --
-- Se puede utilizar la función and que retorna true si todos los elementos de la lista son true
-- paraTodo :: [Bool] -> Bool
-- paraTodo = and
-- ____________________________________________________________________________________________ --
--    b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una
--    lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
{-
ghci> sumatoria [1,2,3]
6
ghci> sumatoria [1,2,3,30,21]
57
-}
--    c ) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de
--    la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
{-
ghci> productoria [1,2,3]
6
ghci> productoria [1,2,3,30,21]
3780
-}
--    d ) factorial :: Int -> Int, que toma un ńumero n y calcula n!.
factorial :: Int -> Int
factorial x = product [1..x]
{-
ghci> factorial 4
24
ghci> factorial 19
121645100408832000
-}
--    e) Utiliza la funcion sumatoria para definir, promedio :: [Int] -> Int, que toma
--    una lista de numeros no vacia y calcula el valor promedio (truncado, usando division
--    entera).
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)
{-
ghci> promedio [1,2,3]
2
ghci> promedio [9,2,8,1,3]
3
-}

-- 3. Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se
-- encuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece k (x:xs) = k == x || pertenece k xs
{-
ghci> pertenece 3 [1,3,2,4]
True
ghci> pertenece 3 [1,2,1,6,8,2,4]
False
-}

-- 4. Programa las siguientes funciones que implementan los cuantificadores generales. Nota que
-- el segundo parametro de cada funcion, es otra funcion!
--    a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
--    predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el
--    predicado t.
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] f = True
paraTodo' (x:xs) f = f x && paraTodo' xs f
{-
ghci> paraTodo' [1,2,3,0] esCero
False
ghci> paraTodo' [0,0,0,0] esCero
True
-}
--    b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
--    predicado t :: a -> Bool, determina si alǵun elemento de xs satisface el predicado
--    t.
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = f x || existe' xs f
{-
ghci> existe' [1,2,3,0] esCero
True
ghci> existe' [8,9,23,4] esCero
False
-}
--    c ) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
--    funcion t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
--    suma de los valores que resultan de la aplicacion de t a los elementos de xs.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f
{-
ghci> sumatoria' [1,2,3,4] factorial
33
-}
--    d ) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
--    y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la
--    aplicacion de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * productoria' xs f
{-
ghci> productoria' [1,2,3,4] factorial
288
-}


-- 5. Definí nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo’ (sin
-- recursion ni analisis por casos!). 
esTrue x = x == True
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paraTodo' xs esTrue
{-
ghci> paratodo'' [True,True,False]
False
ghci> paratodo'' [True,True,True]
True
-}

-- 6. Utilizando las funciones del ejercicio 4, programa las siguientes funciones por composicíon,
-- sin usar recursion ni analisis por casos.
--    a) todosPares :: [Int] -> Bool verifica que todos los numeros de una lista sean
--    pares.
todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs even
{-
ghci> todosPares [1,2,3]
False
ghci> todosPares [0,2,4]
True
ghci> todosPares [6,2,4]
True
-}
--    b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algun numero dentro del
--    segundo parametro que sea multiplo del primer parametro.
esMultiplo :: Int -> Int -> Bool
esMultiplo a b = mod b a == 0
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (esMultiplo n)
{-
ghci> hayMultiplo 2 [1,9,5]
False
ghci> hayMultiplo 2 [1,9,8]
True
-}
--   c ) sumaCuadrados :: Int -> Int, dado un ńumero no negativo n, calcula la suma de
--   los primeros n cuadrados, es decir 〈∑i : 0 ≤i < n : i2〉.
cuadrado :: Int -> Int
cuadrado k = k*k
sumaCuadrados :: Int -> Int
sumaCuadrados k = sumatoria' [0..(k-1)] cuadrado 
{-
ghci> sumaCuadrados 3
5
ghci> sumaCuadrados 8
140
-}
--   d ) Programar la fucion existeDivisor::Int-> [Int] -> Bool, que dado en entero n
--   y una lista ls , devuelve True si y solo si, existe algun elemento en ls que divida a na
esDivisor :: Int -> Int -> Bool
esDivisor n d = n `mod` d == 0
existeDivisor :: Int -> [Int] -> Bool
existeDivisor k xs = existe' xs (esDivisor k)
{-
ghci> existeDivisor 2 [9,9,8]
False
ghci> existeDivisor 2 [9,9,1]
True
-}
--   e) Utilizando la funcion del apartado anterior, definí la función esPrimo:: Int -> Bool,
--   que dado un entero n, devuelve True si y solo si n es primo.
esPrimo :: Int -> Bool
esPrimo n = n > 1 && not (existeDivisor n [2..(n-1)])
{-
ghci> esPrimo 3
True
ghci> esPrimo 4
False
-}
-- f) ¿Se te ocurre como redefinir factorial (ej. 2d ) para evitar usar recursi ́on?
{-factorial :: Int -> Int
factorial x = product [1..x]-}
-- g ) Programar la función multiplicaPrimos :: [Int] -> Int que calcula el producto
-- de todos los números primos de una lista
multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = product (filter esPrimo xs)
-- h) Programar la función esFib :: Int -> Bool, que dado un entero n, devuelve True
-- si y s ́olo si n está en la sucesión de Fibonacci.
fib :: Int -> Int
fib n | n <= 0    = 0
      | n == 1    = 1
      | otherwise = fib (n - 1) + fib (n - 2)
esFib :: Int -> Bool
esFib n = n `elem` [fib i | i <- [0..(n+1)]]
{-
ghci> esFib 5
True
ghci> esFib 10
False
-}
-- i ) Utilizando la funci ́on del apartado anterior, defin ́ı la funci ́on todosFib :: [Int] -> Bool
-- que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen
-- (o no) a la sucesi ́on de Fibonacci.
todosFib :: [Int] -> Bool
todosFib xs = paraTodo' xs esFib
{-
ghci> todosFib [2,3,4,5]
False
ghci> todosFib [3,5,8,89]
True
-}
-- 7. Indag ́a en Hoogle sobre las funciones map y filter. Tambi ́en podes consultar su tipo en
-- ghci con el comando :t.
--                               ¿Qué hacen estas funciones?
{-
MAP es una f de orden superior que toma una función (que a su vez ésta toma un a y un b,) 
y una lista xs y aplica esa función a cada elemento de xs, produciendo una nueva lista.   
    .----------------------------------------. 
    :                                        : 
    :   map :: (a -> b) -> [a] -> [b]        : 
    :   map f [] = []                        : 
    :   map f (x:xs) = f x : map f xs        : 
    :                                        : 
    `----------------------------------------'                                
FILTER es una f que toma un predicado y una lista, 
devolviendo una lista con los elementos que satisfacen el predicado. Es decir, si p x se evalua
en True, x es incluido a la lista.
    .-----------------------------------------------. 
    :                                               :
    :   filter :: (a -> Bool) -> [a] -> [b]         : 
    :   filter p [] = []                            : 
    :   filter p (x:xs) | p x = x : filter p xs     : 
    :                   | otherwise = filter p xs   : 
    :                                               : 
    `------------------------------------------- ---'    
-}
--            ¿A qué equivale la expresión map succ [1, -4, 6, 2, -8], donde succ n = n+1?
{-
Equivale a la lista [2,-3,7,3,-7], donde cada elemento es el siguiente de la lista dada.
-}
--                       ¿Y la expresi ́on filter esPositivo [1, -4, 6, 2, -8]?
--A la lista de los positivos [1,6,2] pertenecientes a la lista dada.


-- 8. Programá una función que dada una lista de números xs, devuelve la lista que resulta de
-- duplicar cada valor de xs.
--    a) Definila usando recursión.
dupLista :: (Num a) => [a] -> [a]
dupLista [] = []
dupLista (x:xs) = (2*x) : dupLista xs
{-
ghci> dupLista [1,2,3,4]
[2,4,6,8]
-}
--    b) Definila utilizando la función map.
dupLista' :: (Num a) => [a] -> [a]
dupLista' xs = map (*2) xs
{-
ghci> dupLista' [1,2,3,4]
[2,4,6,8]
-}

-- 9. Programá una función que dada una lista de números xs, calcula una lista que tiene como
-- elementos aquellos números de xs que son primos.
--    a) Definila usando recursión.
primListas :: [Int] -> [Int]
primListas [] = []
primListas (x:xs) | esPrimo x = x : primListas xs
                  | otherwise = primListas xs
{-
ghci> primListas [1,2,3,4,5,6,7,8,9]
[2,3,5,7]
ghci> primListas [1..100]
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
-}
--    b) Definila utilizando la funci ́on filter.
primListas' :: [Int] -> [Int]
primListas' xs = filter esPrimo xs
{-
ghci> primListas' [1,2,3,4,5,6,7,8,9]
[2,3,5,7]
ghci> primListas' [1..100]
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
-}
--    c) Revisá tu definicián del ejercicio 6g . ¿Cómo podes mejorarla?
{-
multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = product (filter esPrimo xs)
-}


-- 10. La función primIgualesA toma un valor y una lista, y calcula el tramo inicial más largo de
-- la lista cuyos elementos son iguales a ese valor.
--    a) Programá primIgualesA por recursión.
primIgualesA :: (Eq a) => a -> [a] -> [a]
primIgualesA _ []  = []
primIgualesA k (x:xs) | k == x = x : primIgualesA x xs
                      | otherwise = []
{-
ghci> primIgualesA 1 [1,1,2,3,4,1,1,1]
[1,1]
ghci> primIgualesA 2 [1,2,2,2,2,2,2,2]
[]
-}
--    b) Programá nuevamente la función utilizando takeWhile.
primIgualesA' :: (Eq a) => a -> [a] -> [a]
primIgualesA' x = takeWhile (==x)
{-
ghci> primIgualesA' 1 [1,1,2,3,4,1,1,1]
[1,1]
ghci> primIgualesA' 2 [1,2,2,2,2,2,2,2]
[]
-}


-- 11. La funci ́on primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos
-- elementos son todos iguales entre sí.
--    a) Programá primIguales por recursión
primIguales :: (Eq a) => [a] -> [a] 
primIguales [] = []
primIguales (x:xs) | (x == head xs) = x : primIguales xs 
                   | otherwise = x : []
{-
ghci> primIguales [4,4,3,4,4,4,4,4,1]
[4,4]
ghci> primIguales [3,4,3,4,4,4,4,4,1]
[3]
-}
--    b) Usá cualquier versión de primIgualesA para programar primIguales. Está permitido
--    dividir en casos, pero no usar recursián.
primIguales' :: (Eq a) => [a] -> [a]
primIguales' (x:xs) = primIgualesA' x (x:xs)
{-
ghci> primIguales' [4,4,3,4,4,4,4,4,1]
[4,4]
ghci> primIguales' [3,4,3,4,4,4,4,4,1]
[3]
-}


-- 