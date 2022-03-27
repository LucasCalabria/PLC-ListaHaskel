{-
:cd C:\Users\Lucas\Desktop\Scripts\Haskel
:load Lista_3
-}
{-
safeSecond :: [a] -> Maybe a
safeSecond []       = Nothing
safeSecond (a:[])   = Nothing
safeSecond (a:b:c)  = Just b
-}

---------------------------------------------------------------

import Data.Char

import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

main = do
       a <- getLine
       safeCalc a

safeCalc :: String -> IO ()
safeCalc str = putStr (show (op str))

op :: String -> Maybe Int
op str 
    | (getOp str) == "sum"             = Just (toInt (getNum1 str) + toInt (getNum2 str False))
    | (getOp str) == "sub"             = Just (toInt (getNum1 str) - toInt (getNum2 str False))
    | (getOp str) == "mul"             = Just (toInt (getNum1 str) * toInt (getNum2 str False))
    | (toInt (getNum2 str False)) /= 0 = Just (div (toInt (getNum1 str)) (toInt (getNum2 str False)))
    | otherwise                        = Nothing

toInt :: String -> Int
toInt [] = 0
toInt (a:as) = ( fromIntegral (ord a - ord '0') * (10^(length (a:as) - 1))) + toInt as

getNum1 :: String -> String
getNum1 (a:as)
    | (ord a <= ord '9') && (ord a >= ord '0') = a : getNum1 as
    | otherwise = ""

getNum2 :: String -> Bool -> String
getNum2 str True = str
getNum2 (a:b:c:d) False
    | (ord a <= ord '9') && (ord a >= ord '0') = getNum2 (b:c:d) False
    | otherwise = getNum2 d True

getOp :: String -> String
getOp "" = ""
getOp (a:as)
    | (ord a <= ord '9') && (ord a >= ord '0') = getOp as
    | otherwise = a : getOp as

a = "2sub3"