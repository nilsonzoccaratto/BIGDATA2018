--Exercicio 01
matrizIdentidade :: Int -> [[Int]]
matrizIdentidade n = [[fromEnum $ (x == y) | x <- [1..n]]| y <- [1..n]]

--main = do
--print(matrizIdentidade 4)
--print(matrizIdentidade 10)



--Exercicio 02
somaDiagonalPrincipal :: (Num a) => [[a]] -> a
somaDiagonalPrincipal d = sum (somaDiagonalPrincipal d)
      where
      somaDiagonalPrincipal [[]]       = []
      somaDiagonalPrincipal (xs:[])    = [head xs]
      somaDiagonalPrincipal (x:xs)     = head x : somaDiagonalPrincipal (map tail xs)

--main = do
--print $ somaDiagonalPrincipal [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
--print $ somaDiagonalPrincipal [[1,2,5],[2,3,6],[1,2,3]]
--print $ somaDiagonalPrincipal [[1,2],[2,3]]



--Exercicio 03
somaDiagonalSecundaria :: (Num a) => [[a]] -> a
somaDiagonalSecundaria d = sum (somaDiagonalSecundaria $ reverse d)
      where
      somaDiagonalSecundaria [[]]       = []
      somaDiagonalSecundaria (xs:[])    = [head xs]
      somaDiagonalSecundaria (x:xs)     = head x : somaDiagonalSecundaria (map tail xs)

--main = do
--print $ somaDiagonalSecundaria [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
--print $ somaDiagonalSecundaria [[1,2,5],[2,3,6],[1,2,3]]
--print $ somaDiagonalSecundaria [[1,2],[2,3]]
