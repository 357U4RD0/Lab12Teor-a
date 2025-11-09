-- Estuardo Castro 23890
-- Teoría de la computación
-- 9.11.2925

-- Ejercicio 1: Ordenar lista de diccionarios por un campo
data Auto = Auto { brand :: String
                 , model :: String
                 , year :: Int
                 } deriving (Show)

ordenarPorAño :: [Auto] -> [Auto]
ordenarPorAño [] = []
ordenarPorAño [x] = [x]
ordenarPorAño (x:xs) = 
    let menores = ordenarPorAño [a | a <- xs, year a <= year x]
        mayores = ordenarPorAño [a | a <- xs, year a > year x]
    in menores ++ [x] ++ mayores

ejercicio1 :: IO ()
ejercicio1 = do
    let autos = [ Auto "Audi" "Q7" 2019
                , Auto "Ford" "Mustang" 1964
                , Auto "BMW" "X5" 2021
                , Auto "Honda" "Civic" 2011
                ]
    putStrLn "Lista original:"
    print autos
    putStrLn "\nOrdenado por año:"
    print $ ordenarPorAño autos


-- Ejercicio 2: Calcular potencia n-esima de cada elemento
potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x n = x * potencia x (n-1)

aplicarPotencia :: Int -> [Int] -> [Int]
aplicarPotencia n [] = []
aplicarPotencia n (x:xs) = potencia x n : aplicarPotencia n xs

ejercicio2 :: IO ()
ejercicio2 = do
    let numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let n = 2
    putStrLn $ "Lista original: " ++ show numeros
    putStrLn $ "Potencia " ++ show n ++ ":"
    print $ aplicarPotencia n numeros


-- Ejercicio 3: Transpuesta de una matriz
obtenerColumna :: Int -> [[Int]] -> [Int]
obtenerColumna n [] = []
obtenerColumna n (fila:filas) = (fila !! n) : obtenerColumna n filas

transpuesta :: [[Int]] -> [[Int]]
transpuesta [] = []
transpuesta matriz = transponerAux matriz 0 (length (head matriz))
  where
    transponerAux m col maxCol
      | col >= maxCol = []
      | otherwise = obtenerColumna col m : transponerAux m (col+1) maxCol

ejercicio3 :: IO ()
ejercicio3 = do
    let matriz = [[1, 12, 8],
                  [6, 7, 15],
                  [8, 6, 9],
                  [4, 7, 4]]
    putStrLn "Matriz original:"
    mapM_ print matriz
    putStrLn "\nMatriz transpuesta:"
    mapM_ print $ transpuesta matriz


-- Ejercicio 4: Eliminar elementos de una lista
estaEn :: String -> [String] -> Bool
estaEn x [] = False
estaEn x (y:ys) = if x == y then True else estaEn x ys

eliminar :: [String] -> [String] -> [String]
eliminar [] aBorrar = []
eliminar (x:xs) aBorrar = 
    if estaEn x aBorrar
    then eliminar xs aBorrar
    else x : eliminar xs aBorrar

ejercicio4 :: IO ()
ejercicio4 = do
    let colores = ["rojo", "verde", "azul", "amarillo", "gris", "blanco", "negro"]
    let aBorrar = ["amarillo", "cafe", "blanco"]
    putStrLn $ "Lista original: " ++ show colores
    putStrLn $ "Elementos a borrar: " ++ show aBorrar
    putStrLn $ "Lista resultante: " ++ show (eliminar colores aBorrar)


main :: IO ()
main = do
    putStrLn "=== Ejercicio 1 ==="
    ejercicio1
    putStrLn "\n=== Ejercicio 2 ==="
    ejercicio2
    putStrLn "\n=== Ejercicio 3 ==="
    ejercicio3
    putStrLn "\n=== Ejercicio 4 ==="
    ejercicio4