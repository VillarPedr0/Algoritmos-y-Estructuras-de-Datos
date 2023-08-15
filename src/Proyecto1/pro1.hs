-- 1. Program ́a las siguientes funciones:
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
esVocal x = x `elem` "aeiouAEIOU"
{-
ghci> esVocal 'a'
True
ghci> esVocal 'v'
False
ghci> esVocal 'U'
True
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
--    d ) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
--    y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la
--    aplicacion de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * productoria' xs f


-- 5. Definı nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo’ (sin
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
esPrimo n | n <= 1 = False
          | otherwise = not (existeDivisor n [2..(n-1)])
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
-- h) Programar la funci ́on esFib :: Int -> Bool, que dado un entero n, devuelve True
-- si y s ́olo si n est ́a en la sucesi ́on de Fibonacci.
fib :: Int -> Int
fib n | n <= 0    = 0
        | n == 1    = 1
        | otherwise = fib (n - 1) + fib (n - 2)
esFib :: Int -> Bool
esFib n = n `elem` [fib i | i <- [0..]]