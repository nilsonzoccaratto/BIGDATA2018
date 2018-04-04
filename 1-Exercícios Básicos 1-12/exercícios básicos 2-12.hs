--Exercício 02
mult3 :: Integer -> Bool  
mult3 x = (x  `mod` 3) == 0


--Exercício 02 com Stringmulti3 :: Integer -> String  
multi3 x = if (x `mod` 3) == 0 then "True" else "False"


--Exercício 03
mult5 :: Integer -> Bool   
mult5 x = (x `mod` 5) == 0


--Exercício 04
mult35 :: Integer -> Bool   
mult35 x = (x `mod` 3) == 0 && (x `mod` 5) == 0


--Exercício 05
exerc5 :: Integer -> Bool  
exerc5 x = (x < (-1)) || (x > 1 && (x `mod` 2 == 0))


--Exercício 06 
div2d :: Integer -> Float 
div2d x = fromInteger x / 2


--Exercício 07
senoMetadeAngulo :: Integer -> (Double, Double)  
senoMetadeAngulo x = ( sqrt ((1 - cos (fromIntegral x / 2)) / 2) , - sqrt ((1 - cos (fromIntegral x / 2)) / 2) )


--Exercício 08
listaBissextos = [ ano | ano <- [1..2018], (ano `mod` 400 == 0) || ((ano `mod` 4 == 0) && (ano `mod` 100 /= 0))] 



-- Exercício 09: Encontrar os 10 primeiros anos bissextos: usando as instruções abaixo:
 *Main> take 10 listaBissextos
  [4,8,12,16,20,24,28,32,36,40]



-- Exercício 9b: Encontrar os 10 últimos anos bissextos: usando as instruções abaixo:
*Main> length listaBissextos
489
*Main> drop 479 listaBissextos
[1980,1984,1988,1992,1996,2000,2004,2008,2012,2016]



-- Exercício 10: Criar uma tupla com dois elementos que contenham a metade dos anos bissextos cada uma.
tuplaDivideLista :: [a] -> ([a],[a])
tuplaDivideLista lista = splitAt ( (length lista) `div`2 ) lista 
listaBissextos = [ ano | ano <- [1..2018], (ano `mod` 400 == 0) || ((ano `mod` 4 == 0) && (ano `mod` 100 /= 0))]
*Main> print(tuplaDivideLista listaBissextos)



--Exercício 11: concatenador de duas strings separadas por espaço
concatena :: String -> String -> String
concatena string1 string2 = string1 ++ " " ++ string2
main = do
	let x = "Nilson"
	let y = "Jose"
	let z = "Zocca"
 print (concatena x y)
 print (concatena x z)
 print (concatena x z)



 --Exercício 12: Criar uma lista Integer a partir de um formato String de dígitos
stringPinteger x = read "0123456789":: Integer 

Prelude> putStr "0123456789"
0123456789Prelude> 
