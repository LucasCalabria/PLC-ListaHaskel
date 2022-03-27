{-
:cd C:\Users\Lucas\Desktop\Scripts\Haskel
:load Lista_1
-}

import Data.Char
logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

--Q1

logMes :: String -> String -> Double
logMes month log = foldl (+) 0 (logMesAux month log )

logMesAux :: String -> String -> [Double]
logMesAux _ [] = []
logMesAux month log
    | (getMonth log False) == month = getNum log : logMesAux month (nextEntry log)
    | otherwise                     = logMesAux month (nextEntry log)

nextEntry :: String -> String
nextEntry str = dropWord1 (dropWord1 (dropWord1 str))

getMonth :: String -> Bool -> String -- Primeira chamada comecar com False
getMonth []  _ = []
getMonth (a:as) x
    | x == False = getMonth (dropSpace (dropWord (a:as))) True
    | a == ';'   = ""
    | otherwise  = a : getMonth as True

getNum :: String -> Double
getNum []  = 0
getNum str = 
    let num = getWord1 (dropWord1 (dropWord1 str))
    in  (toInt (getWord2 num)) + (toDouble (dropWord2 num) 1)

toDouble :: String -> Int -> Double
toDouble [] _     = 0
toDouble (a:as) n = (fromIntegral (ord a - ord '0') / (10^n)) + toDouble as (n+1)

toInt :: String -> Double
toInt [] = 0
toInt (a:as) = ( fromIntegral (ord a - ord '0') * (10^(length (a:as) - 1))) + toInt as

dropSpace :: String -> String
dropSpace "" = ""
dropSpace txt@(c:cs) 
    | c == ' ' = dropSpace cs
    | otherwise = txt

dropWord :: String -> String --Descarta ate ' '
dropWord "" = ""
dropWord (c:cs) 
    | c /= ' ' = dropWord cs
    | otherwise = c:cs

dropWord1 :: String -> String --Descarta ate ';'
dropWord1 "" = ""
dropWord1 (c:cs) 
    | c /= ';' = dropWord1 cs
    | otherwise = cs

dropWord2 :: String -> String --Descarta ate '.'
dropWord2 "" = ""
dropWord2 (c:cs) 
    | c /= '.' = dropWord2 cs
    | otherwise = cs

getWord :: String -> String --Pega ate ' '
getWord "" = ""
getWord (c:cs) 
    | c == ' ' = ""
    | otherwise = c : getWord cs

getWord1 :: String -> String --Pega ate ';'
getWord1 "" = ""
getWord1 (c:cs) 
    | c == ';' = ""
    | otherwise = c : getWord1 cs

getWord2 :: String -> String --Pega ate '.'
getWord2 "" = ""
getWord2 (c:cs) 
    | c == '.' = ""
    | otherwise = c : getWord2 cs


--teste str@(a:m1:m2:m3:as) = m2

--Q2

--Q3
isReplica :: String -> Int -> Char -> Bool
isReplica [] n _        | n == 0    = True
                        | otherwise = False

isReplica (a:as) n ch   | n < 0     = False 
                        | a /= ch   = False
                        | otherwise = isReplica as (n-1) ch

--Q4
decEnigma :: String -> [(Char, Char)] -> String
decEnigma str []          = str
decEnigma str ((a, b):bs) = decEnigma (swap str a b) bs

swap :: String -> Char -> Char -> String
swap [] _ _     = []
swap (a:as) n m | a == n    = m : swap as n m
                | otherwise = a : swap as n m

--Q5
btoi :: String -> Int
btoi []     = 0
btoi (a:as) = ((ord a - ord '0') * (2^(length as))) + btoi as

--Q6
type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa a = executaAux a 0

executaAux :: [(Comando, Valor)] -> Int -> Int
executaAux [] x = x
executaAux ((cmd,val):as) x | cmd == "Multiplica" = executaAux as ( x * val)
                            | cmd == "Soma"       = executaAux as ( x + val)
                            | cmd == "Subtrai"    = executaAux as ( x - val)
                            | cmd == "Divide"     = if val /= 0 then executaAux as (div x val)
                                                    else -666

--Q7
mul2 :: [Int] -> [Int] -> [Int]
mul2 [] []          = []
mul2 [] (b:bs)      = 0 : mul2 bs []
mul2 (a:as) []      = 0 : mul2 as []
mul2 (a:as) (b:bs)  = (a*b) : mul2 as bs

--------------------------------------------------------

splitWords :: String -> [String]
splitWords "" = []
splitWords texto = 
    let textoOK = dropSpace texto
    in (getWord textoOK) : splitWords (dropSpace (dropWord textoOK))


