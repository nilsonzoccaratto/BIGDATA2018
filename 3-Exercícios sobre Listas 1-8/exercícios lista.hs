--Exercício 01
divisivel20 :: Integer -> Bool
divisivel20 x
  | (length $ [y | y <- lista, x `mod` y == 0]) == length(lista) = True
  | otherwise = False

lista = [1..20]



--Exercício 02
projectEuler5 = foldr1 lcm lista

--lista = [1..20]
--Main> projectEuler5 
--232792560



--Exercício 03
fibonacci :: [Integer]
fibonacci = 1 : 2 : prox fibonacci
  where
    prox (x:t@(y:resto)) = (x+y) : prox t

--main = do
--print fibonacci



--Exercício 04
projectEuler2 = sum listaFibonacci
  where
    listaFibonacci = takeWhile (<= 4000000) paresFibonacci  
    paresFibonacci = [x | x <- fibonacci, even x]
  
--main = do
--print somaFibonacci



--Exercício 05
produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar [x,xs] [y,ys] = (x * y) + (xs * ys)

--main = do 
--Main> print (produtoEscalar [2,7] [-3,8])
--50



--Exercício 06
collatz :: Integer -> Integer
collatz x 
  | even x == True = div x 2
  | otherwise = 3 * x + 1



--Exercício 07
collatzLista:: Int -> [Int]
collatzLista 1 = [1]
collatzLista x = x:(collatzLista (collatz x))

collatzLen:: Int -> Int
collatzLen x = length (collatzLista x)



--Exercício 08
projectEuler14 :: [Int] -> Int
projectEuler14 lista = pegaElemento $ (maximum [(length $ collatzLista x, head $ collatzLista x) | x <- lista])
  where
    pegaElemento (x,y) = y

--main = do 
  --print (projectEuler14 [1..10])







