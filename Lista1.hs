module Lista1 where

import Data.Maybe



verificaChar :: Char -> String 
verificaChar c = if c >= 'a' && c <= 'z' then "e minusculo"
    else if c >= 'A' && c <= 'Z'
	    then "e maisculo"
	    else "Nao e um caracter"


absoluto :: Int -> Int
absoluto x | x>=0 	= x
		   | otherwise = -x

antcs :: Int -> Int
antcs x | x>0 	= x-1
		| otherwise = x

{- testando variaveis anonimas-}




type Seq = String
type Nomes = (Seq, Seq, Seq, Seq)
--funções constantes
f_nomes_estacoes :: Nomes
f_nomes_estacoes = ("Inverno","Outono","Primavera","Verao")

primeiro :: Nomes -> Seq
primeiro (x, _, _, _) = x
segundo :: Nomes -> Seq
segundo (_, x, _, _) = x

dvt3 :: Int->Int->Int -> Int
dvt3 x y z | x==7		= 10
		  | y==8 	= 20
		  | z==9 	= 30
		  | otherwise = 0 

andF :: Bool -> Bool -> Bool
andF False _ = False
andF _ False = False
andF True True = True

orF :: Bool -> Bool -> Bool 
orF False False = False
orF _ True = True
orF True _ = True

comDesconto :: Double -> Double
comDesconto valor | valor < 50.0 = valor
				  | valor >= 50.0 && valor < 100.0 = valor - valor * 0.05
				  | valor >= 100.00 && valor < 300 = valor - valor * 0.1
				  | otherwise = valor - valor * 0.15

potDois :: Int -> Int
potDois n | n == 0 = 1
		  | otherwise = (potDois(n-1)) + (potDois(n-1))

eTriangulo :: Int -> Int -> Int -> String
eTriangulo x y z | x > y+z || y > x+z || z > x+y = "Nao é um triangulo !"
				 | x == y && y == z && z == x = "E um triangulo Equilatero"
				 | x == y || y == z || z == x = "E um triangulo Isosceles"
				 | x /= y && y /= z && z /= x = "E um triangulo Escaleno"

type Convertido = (Float, String)
realConvertido :: Float -> (Convertido,Convertido,Convertido)
realConvertido real =  ((real,"Real"),(real *0.19, "Dolar"),(real *0.195, "Euro"))
	
--11 
n = 3
m = 5
mult3v5 n m = n + (mult3v5 n (m - 1))

-- Numero perfeito 12

fatores :: Int -> [Int]
fatores num  = [ x | x <- [1 .. num - 1], ((mod num x) == 0)]

ePerfeito :: Int -> Bool
ePerfeito num 
			| ((sum (fatores num)) == num ) = True
			| otherwise = False

sequenciaN :: Int -> [Int]
sequenciaN 1 = [1]
sequenciaN num 
	|mod num 2 == 0 = num:sequenciaN (div num 2)
	|otherwise = num:sequenciaN (3*num+1) 

-- 13 passar sequenciaN como parametro quando chamar a função tamCiclo
tamCiclo :: [Int] -> Int
tamCiclo [] = 0
tamCiclo (_:tl) = 1 + tamCiclo tl

-- 14 

divisoresComum :: Int -> Int -> [Int]
divisoresComum a b = [x | x <- [1 .. a], mod a x == 0, mod b x == 0]

mdc a b = maximum(divisoresComum a b)