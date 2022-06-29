module Lista2 where

import Data.Maybe

-- Definindo uma lista de testes
lista = [3,4,5,8,9,2]

-- 1 
listHead :: [a] -> a
listHead (x:_) = x 
listHead [] = error "Lista vazia"

-- 2
listTail [x] = x
listTail (_:xs) = listTail xs
listTail [] = error "Lista vazia"

-- 3
tresFirst::[Int]->Int
tresFirst lista= sum(getF 3 lista)

getF:: Int -> [Int] -> [Int]
getF 0 _ = []
getF _ [] = []
getF n (h:t) = h : getF (n-1) t

-- 4
pertence [] n = False
pertence (h:t) n = if h == n then True
                      else pertence t n

-- 5
zipar (h:t) (h2:t2) = [(h,h2)] ++ zipar t t2
zipar [] [] = []


--6
dobra :: [Int]->[Int]
dobra [] = []
dobra (h:t) = h * 2 : dobra t

-- 7
andList :: [Bool] -> Bool
andList (h:t) = h && andList t
andList [] = True

-- 8
-- valor, posicao, lista, novaLista
inserirX :: a -> Int -> [a] -> [a]
inserirX novo _ [] = [novo]
inserirX novo i (x:xs) | i <= 0 = novo:x:xs
                       | otherwise = x : inserirX novo (i - 1) xs
-- 9
insereUlt n [] = [n]
insereUlt n (h: t) = if h /= n then (h: insereUlt n t)
                      else (h: t)
-- 10
maior :: [Int] -> Int
maior [a] = a
maior (h:t) = if h > maior t then h
                else maior t
-- 11a
mulTres :: [Integer]
mulTres = [x | x <- [0 .. 15], x `mod` 3 == 0]

-- 11b
multDois :: [Int]
multDois = [x | x <- [0 .. 20], x `mod` 2 == 0]

-- 11c 
listaDlista :: [[Int]]
listaDlista = [[x] | x <- [1 .. 5]]

-- 11d 
listaDunsAux :: (Eq t, Num t, Num a) => t -> [a]
listaDunsAux 0 = []
listaDunsAux n = 1 : listaDunsAux (n - 1)

listaDuns :: [[Integer]]
listaDuns = [listaDunsAux x | x <- [1 .. 5]]

-- 11e
listaDtuplas :: [(Int,Int)]
listaDtuplas = [(h, t) | h <- [1,2,3], t <- [3,2,1]]

-- 12 
retornaSup::Ord a=> a->[a]->[a]
retornaSup n lista = filter (> n) lista

-- 14
tabuada :: Int -> [(Int, Int, Int)]

tabuada n = [(n, x, n*x) | x <- [1..10]]

-- 15

sequencia 0 m = []
sequencia n m = m: sequencia (n-1) (m+1)