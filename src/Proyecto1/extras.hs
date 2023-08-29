-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Programar la función tocaListaN que recibe un número entero n y una lista de enteros y 
-- devuelve la menor cantidad de números tal que la suma de estos sea igual a n.
tocaListaN :: [Int] -> Int -> Int
tocaListaN [] _ = 0
tocaListaN
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.1. Definir la función factorial tal que factorial n es el factorial de n.
-- ◆ Forma 1: Utilizando Condicionales
fact1 :: Integer -> Integer
fact1 n = if n == 0 then 1
                    else n * fact1 (n-1)
-- ◆ Forma 2: Utilizando Guardas
fact2 :: Integer -> Integer
fact2 n
| n == 0 = 1
| otherwise = n * fact2 (n-1)
-- ◆ Forma 3: Utilizando pattern matching
fact3 :: Integer -> Integer
fact3 0 = 1
fact3 n = n * fact3 (n-1)
-- ◆ Forma 4: Utilizando reestricción de dominio mediante guardas
fact4 :: Integer -> Integer
fact4 n
| n == 0 = 1
| n >= 1 = n * fact4 (n-1)
-- ◆ Forma 5: Utilizando reestricción de dominio mediante pattern matching
fact5 :: Integer -> Integer
fact5 0 = 1
fact5 (n+1) = (n+1) * fact5 n
-- ◆ Forma 6: Utilizando funcion predefinida en haskell
fact6 :: Integer -> Integer
fact6 n = product [1..n]
-- ◆ Forma 7: Utilizando foldr
fact7 :: Integer -> Integer
fact7 n = foldr (*) 1 [1..n]
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.2. Definir la función comb tal que comb n k es el número de combinaciones de n
-- elementos tomados de k en k.
comb :: Int -> Int
comb n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.3. Definir la función impar tal que impar x se verifica si el número x es impar. 
-- ◆ Forma 1: Utilizando función predefinida odd
impar1 :: Integer -> Bool
impar1 = odd
-- ◆ Forma 2: Utilizando función predefinida even y negación
impar2 :: Integer -> Bool
impar2 x = not (even x)
-- ◆ Forma 3: Utilizando recursión
impar3 :: Integer -> Bool
impar3 x | x > 0 = impar3_aux x
         | otherwise = impar3_aux (-x)
         where impar3_aux 0 = False
            impar3_aux 1 = True
            impar3_aux (n+2) = impar3_aux n
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.4. Definir la función cuadrado tal que cuadrado x es el cuadrado del número x.
-- ◆ Forma 1: Utilizando (*)
cuadrado_1 :: Num a => a -> a
cuadrado_1 x = x*x
-- ◆ Forma 2: Utilizando (^)
cuadrado_2 :: Num a => a -> a
cuadrado_2 x = x^2
-- ◆ Forma 3: Utilizando secciones
cuadrado_3 :: Num a => a -> a
cuadrado_3 = (^2)
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.5. Definir la función suma_de_cuadrados tal que suma_de_cuadrados xs es la
-- suma de los cuadrados de los elementos de la lista xs.
-- ◆ Forma 1: Utilizando sum, map y cuadrado
suma_de_cuadrados_1 :: [Integer] -> Integer
suma_de_cuadrados_1 l = sum (map cuadrado l)
-- ◆ Forma 2: Utilizando sum y lista por comprensión
suma_de_cuadrados_2 :: [Integer] -> Integer
suma_de_cuadrados_2 l = sum [x*x | x <- l]
-- ◆ Forma 3: Utilizando sum, map y lambda
suma_de_cuadrados_3 :: [Integer] -> Integer
suma_de_cuadrados_3 l = sum (map (\x -> x*x) l)
-- ◆ Forma 4: Utilizando recursión
suma_de_cuadrados_4 :: [Integer] -> Integer
suma_de_cuadrados_4 [] = 0
suma_de_cuadrados_4 (x:xs) = x*x + suma_de_cuadrados_4 xs
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.6. Definir la función raices tal que raices a b c es la lista de las raices de la
-- ecuación ax2 + bc + c = 0.
-- ◆ Forma 1
raices_1 :: Double -> Double -> Double -> [Double]
raices_1 a b c = [ (-b+sqrt(b*b-4*a*c))/(2*a),
                   (-b-sqrt(b*b-4*a*c))/(2*a) ]
-- ◆ Forma 2: Definiendo variables locales
raices_2 :: Double -> Double -> Double -> [Double]
raices_2 a b c =
    [(-b+d)/n, (-b-d)/n]
    where d = sqrt(b*b-4*a*c)
        n = 2*a
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.7. Redefinir la función abs tal que abs x es el valor absoluto de x.
-- ◆ Forma 1: Utilizando condicionales
n_abs_1 :: (Num a, Ord a) => a -> a
n_abs_1 x = if x>0 then x else (-x)
-- ◆ Forma 2: Utilizando guardas
n_abs_2 :: (Num a, Ord a) => a -> a
n_abs_2 x | x>0 = x
          | otherwise = -x
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.8. Redefinir la función signum tal que signum x es -1 si x es negativo, 0 si x es cero
-- y 1 si x es positivo.
n_signum :: Int -> Int 
n_signum x | x > 0 = 1
           | x == 0 = 0
           | otherwise = -1
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.9. Redefinir la función && tal que x && y es la conjunción de x e y
(&&&) :: Bool -> Bool -> Bool
False &&& x = False
True &&& x = x
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.10. Definir la función anterior tal que anterior x es el anterior del número
-- natural x
-- ◆ Forma 1: Utilizando pattern matching
anterior_1 :: Int -> Int
anterior_1 (n+1) = n
-- ◆ Forma 2: Utilizando guardas
anterior_2 :: Int -> Int
anterior_2 n | n>0 = n-1
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.11. Redefinir la función potencia tal que potencia x y es x^y
-- ◆ Forma 1: Utilizando pattern matching
potencia_1 :: Num a => a -> Int -> a
potencia_1 x 0 = 1
potencia_1 x (n+1) = x * (potencia_1 x n)
-- ◆ Forma 2: Utilizando condicionales
potencia_2 :: Num a => a -> Int -> a
potencia_2 x n = if n==0 then 1
                 else x * potencia_2 x (n-1)
-- ◆ Forma 3: Programación eficiente
potencia_3 :: Num a => a -> Int -> a
potencia_3 x 0 = 1
potencia_3 x n | n > 0 = f x (n-1) x
               where f _ 0 y = y
                     f x n y = g x n
                        where g x n | even n = g (x*x) (n`quot`2)
                                    | otherwise = f x (n-1) (x*y)
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ --

-- ╔═══════════════════════════════════════════════════════════════════════════════════════════╗ --
-- Ejercicio 1.12. Redefinir la función id tal que id x es x.
n_id :: a -> a
n_id x = x
-- ╚═══════════════════════════════════════════════════════════════════════════════════════════╝ -- 