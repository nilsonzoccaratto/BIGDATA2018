--Exercício 01
ehTriangulo :: Integer -> Integer -> Integer -> Bool
ehTriangulo x y z
  | (x > abs (y - z)) && (x < y + z) = True
  | (y > abs (x - z)) && (y < x + z) = True
  | (z > abs (x - y)) && (z < x + y) = True
  | otherwise = False



--Exercício 02
tipoTriangulo :: Integer -> Integer -> Integer -> String
tipoTriangulo x y z
  | x == y && x == z && y == z = "Equilatero"
  | x /= y && x /= z && y /= z = "Escaleno"
  | otherwise = "Isosceles"



--Exercício 03
multiEtiope :: Integer -> Integer -> Integer
multiEtiope m n = multiEtiope' m n 0
  where
    multiEtiope' m n r
     | m == 1 = n + r
     | even m == True = multiEtiope' (m `div` 2) (n*2) r
     | otherwise = multiEtiope' (m `div` 2) (n*2) (r+n)



--Exercício 04
ehPrimo :: Integer -> Bool
ehPrimo 0 = False
ehPrimo 1 = False
ehPrimo 2 = True
ehPrimo numero 
       | (length [x | x <- [2 .. numero], mod numero x == 0]) == 1 = True
       | otherwise = False



--Exercício 05
somaDigitos :: Integer -> Integer
somaDigitos x 
  | x < 10 = x
  | x >= 10 = ( x `div` 10 + x `rem` 10)



--Exercício 06
persistenciaAditiva :: Integer -> Integer
persistenciaAditiva x 
  | x < 10 = 0
  | otherwise = persistenciaAditiva(sum $ digitos x) + 1

digitos :: Integer -> [Integer]
digitos n = map (\x -> read [x] :: Integer) (show n)



--Exercício 07
fatorial :: Integer -> Integer
fatorial n = product [1..n]

coeficienteBinomial :: Integer -> Integer -> Integer
coeficienteBinomial m n = (fatorial m) `div` (fatorial n * fatorial (m - n))



--Exercício 08
--fatorial :: Integer -> Integer 
--fatorial n = product [1..n]
--vai como comentario para não dar erro de compilação - duplicação - já que essas funções estão no mesmo programa
elementoTrianguloPascal :: Integer -> Integer -> Integer
elementoTrianguloPascal i j = (fatorial i) `div` (fatorial j * fatorial (i - j))




