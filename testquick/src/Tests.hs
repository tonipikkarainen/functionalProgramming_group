module Tests where
import Test.QuickCheck
import Data.Char
import Data.Word
import Data.List hiding (delete)                   -- .. different lengths of integers



-- CASE 1 
-- Onko upperFirst (upperFirst x) == upperFirst x ?
-- Ajattelin, että on.
prop_upperFirst :: String -> Bool
prop_upperFirst = \x -> upperFirst (upperFirst x) == upperFirst x

-- +++ OK, passed 100 tests.


upperFirst :: String -> String   
upperFirst str = case dropWhile (==' ') str of
    "" -> takeWhile (==' ') str
    x:xs -> takeWhile (==' ') str++toUpper x:(takeWhile (/=' ') xs) 
        ++ upperFirst(dropWhile (/=' ') xs) 


-- CASE 2
-- Onko length (destutter x) < length (x) ?
-- Arvelin, että ei.
prop_destutter :: Eq a => [a] -> Bool
prop_destutter x = length (destutter x) < length (x)  

-- *** Failed! Falsified (after 1 test):  
-- []

destutter :: Eq a => [a] -> [a]
destutter [] = []
destutter zs = case zs of
    x:[] -> x:[] 
    x:xs -> 
        let
            seuraava:ys = xs
        in
            if x == seuraava
                then destutter xs
                else x:(destutter xs)



-- CASE 3
-- Onko kaanna (kaanna x) == x?
-- Arvelin, että on.

prop_kaanna :: Eq a => [a] -> Bool 
prop_kaanna x = kaanna (kaanna x) == x

-- +++ OK, passed 100 tests.

-- CASE 4
-- Onko kaanna (x++y) == kaanna (y++x)??
-- Tämä oli outo, kun meni läpi, mutta
-- sitten tajusin, että lista on ilmeisesti sama
-- kun siinä on samat alkiot!! Eli ei tarvitse olla samassa
-- järjestyksessä!!
prop_kaanna_summa :: Eq a => [a] -> [a] -> Bool 
prop_kaanna_summa x y = kaanna (x++y) == kaanna (y++x)

-- +++ OK, passed 100 tests.

-- CASE 5
-- Onko head(kaanna (x++y)) == head(kaanna (y++x))?
-- Tästä sentään tuli failed.
-- Halusin vaan varmistua edellisen kohdan hypoteesista listojen
-- kanssa.
prop_kaanna_eka_alkio :: Eq a => [a] -> [a] -> Bool 
prop_kaanna_eka_alkio x y = head(kaanna (x++y)) == head(kaanna (y++x))

-- *** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
-- []
-- []

kaanna:: Eq a => [a] -> [a]
kaanna as = case as of
    [] -> []
    [x] -> [x]
    (x:xs) -> (kaanna xs) ++ [x]

-- CASE 6 
-- onko length (delete x xs) <= length xs?
-- Arvelin, että pitäisi olla.
prop_delete_pituus :: Eq a => a -> [a] -> Bool
prop_delete_pituus x xs = length (delete x xs) <= length xs

-- +++ OK, passed 100 tests.

-- CASE 7
-- xs == x:(delete x xs) 
-- Eli, onko sama asia ottaa xs ja xs, josta on poistettu
-- x ja lisätty x.
-- Kuvittelin, että on.
prop_del_jalisays::  Eq a => a -> [a] -> Bool
prop_del_jalisays x xs = xs == x:(delete x xs) 

-- *** Failed! Falsified (after 1 test):  
-- ()
-- []

delete ::Eq a => a -> [a] -> [a]
delete y xs = case xs of
    [] -> []
    (z:zs) -> if z==y then zs else z:(delete y zs)

-- CASE 8 
-- onko decode (encode x) == x?
-- Arvelin, että on ja oli.
prop_en_de ::  String -> Bool
prop_en_de x = decode (encode x) == x

-- +++ OK, passed 100 tests.

encode :: String -> [(Char,Word8)] 
encode []=[] 
encode xs= 
    let 
        (y:zs):ys =group xs
        eka_pituus = min 255 (length (y:zs))
        (alku, loput) = splitAt eka_pituus xs
    in 
        (y, fromIntegral eka_pituus):(encode loput) 

decode :: [(Char,Word8)] -> String
decode [] = []
decode ((x,y):xs) =(replicate (fromEnum y) x)++(decode xs)


-- CASE 9
-- Onko prop_del_last_pituus x xs < xs ?
-- Ajattelin, että ei.
prop_del_last_pituus :: Eq a => a -> [a] -> Bool
prop_del_last_pituus x xs = length(deleteLast x xs) < length xs 

-- *** Failed! Falsified (after 1 test):  
-- ()
-- []

deleteLast :: Eq a => a -> [a] -> [a]
deleteLast x [] = []
deleteLast x (y:[]) = if x==y then [] else (y:[])
deleteLast x ys = 
    let 
        (etu, taka) = deal ys
    in
        if helper x taka 
            then etu ++ (deleteLast x taka)
            else (deleteLast x etu) ++ taka
   where
    helper z zs = case zs of
        [] -> False
        (w:ws) -> if z == w then True else (helper z ws) 
    deal xs = if even (length xs) 
        then splitAt ((length xs) `div` 2 ) xs  
        else splitAt ((length xs) `div` 2 + 1) xs 


-- CASE 10
-- Onko unwords (words x) == x?
-- Ajattelin, että on.
prop_words x = unwords (words x) == x

-- *** Failed! Falsified (after 4 tests and 3 shrinks):     
--" "

-- CASE 11
-- Onko words (unwords x) == x?
-- Ajattelin, että tämäkin on.
prop_unwords x = words (unwords x) == x

-- *** Failed! Falsified (after 2 tests):                  
-- [""]

-- CASE 12 
-- Onko reverse (group x) == group (reverse x)?
-- Ajattelin, että pitäisi olla totta
prop_commut x = reverse (group x) == group (reverse x)

-- +++ OK, passed 100 tests.