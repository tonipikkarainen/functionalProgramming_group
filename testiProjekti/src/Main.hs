module Main where
import Data.List

deal :: [a] -> ([a],[a])
deal lista = let
    a = (length lista) `div` 2
    in ( (take a lista), (drop a lista))

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (x:xs) = x:xs
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys) = if x < y then x:(merge (xs) (y:ys)) else y:(merge (x:xs) (ys))

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (xs) = merge (mergeSort a) (mergeSort b)
        where (a,b) = deal (xs)

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [[]] = []
myConcat [[a]] = [a]
myConcat (x:xs) = x ++ myConcat xs

takeEvens :: [Int] -> [Int]
takeEvens [] = []
takeEvens (x:xs) = if odd x then (takeEvens xs) else (x:(takeEvens xs))


--delete a [] = []
--delete a (x:xs) = if x==a then xs else (x:(delete a xs))

destutter :: Eq a => [a] -> [a]
destutter [] = []
destutter [x] = [x]
destutter (x1:(x2:xs)) = if x1==x2 then (destutter (x2:xs))
                                    else x1:(destutter (x2:xs))



main :: IO ()
main = do
  putStrLn "hello world"
