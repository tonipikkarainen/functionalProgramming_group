module Tests where
import Test.QuickCheck
import Data.List hiding (partition, delete)
import Data.Word                  
import Data.Char (ord,chr)


-- CASE 1
-- Is length x == length y + length z where (y,z) = partition f x?
-- Originally I thought this would hold.

prop_partition :: [Int] -> Bool
prop_partition = \x -> length x == length y + length z where (y,z) = partition even x

-- TEST: > quickCheck prop_partition
-- +++ OK, passed 100 tests.
-- So, this property holds.

-- CASE 2

-- Is length ys >= length (deleteLast x ys)?
-- Originally I thought this would hold.
       
prop_deleteLast :: Eq a => a -> [a] -> Bool
prop_deleteLast = \x ys -> length ys >= length (deleteLast x ys)

-- TEST: > quickCheck prop_deleteLast
-- +++ OK, passed 100 tests.
-- So, this property holds.

-- CASE 3

-- Does the order of the input list matter for mergeSort?
-- Originally we thought that it would not matter.

prob_mergeSortOrderOfTheListDoesNotMatter :: Ord a => [a] -> Bool
prob_mergeSortOrderOfTheListDoesNotMatter = \xs -> (mergeSort xs == mergeSort (reverse xs))

-- This works as we thought.

-- CASE 4

-- Does mergeSort conserve the length of the input list?
-- Originally we thought it should conserve the length.

prob_mergeDoesNotAddNorRemove :: Ord a => [a] -> [a] -> Bool
prob_mergeDoesNotAddNorRemove = \xs ys -> (length (merge xs ys) == (length xs) + (length ys))

-- This works as we thought.

-- CASE 5

-- Does "deal" add or remove elements?
-- Originally we thought it would not remove elements.

prob_dealDoesNotAddNorRemove:: [a] -> Bool
prob_dealDoesNotAddNorRemove = \xs -> let (zs,ys) = deal xs in length xs == ((length zs) + (length ys))
		
-- This works as we thought.

-- CASE 6

-- Is mergeSort idempotent?
-- Originally we thought it would be idempotent.

prob_mergeSortIdempotent :: Ord a => [a] -> Bool
prob_mergeSortIdempotent = \xs -> (mergeSort xs == mergeSort (mergeSort xs))

-- This works as we thought.

-- CASE 7

-- Does reverse (reverse xs) give xs?
-- Originally we thought it would.

prop_reverse :: Eq a => [a] -> Bool
prop_reverse = \xs -> (reverse (reverse xs) == xs)

-- This works as we thought.
		
-- CASE 8
        
-- Does `destutter xs == destutter (destutter xs)`?
-- Originally we thought this would hold.
			
prob_destrutterIdempotent :: Eq a => [a] -> Bool
prob_destrutterIdempotent = \xs -> (destutter xs == destutter (destutter xs))

-- This works as we thought.

-- CASE 9

-- Does lines (unlines x) give x?
-- Originally we thought this would not hold because some of the strings in x might contain "\n".

prob_linesUnlines :: [String] -> Bool
prob_linesUnlines = \x -> lines (unlines x) == x

-- quickCheck prob_linesUnlines
-- *** Failed! Falsified (after 13 tests and 7 shrinks):
-- ["\n"]
-- This works as we thought.

-- CASE 10 

-- Does decode (encode x) give x?
-- Originally we thought this would hold.
                    
prop_RLE :: String -> Bool
prop_RLE = \x -> decode (encode x) == x 

-- This works as we thought.

-- CASE 11

-- Does prod (xs ++ ys) == prod xs * prod ys hold?
-- Originally we thought this would hold.

prop_prod :: [Integer] -> [Integer] -> Bool
prop_prod = \xs ys -> prod (xs ++ ys) == prod xs * prod ys 

-- This works as we thought.

-- CASE 12

-- Is x++y++x a palindrome if x and y are palindromes?
-- Originally we thought this would hold.

prop_palindrome :: String -> String -> Bool
prop_palindrome = \x y -> ((isPalindrome x && isPalindrome y) == isPalindrome (x++y++x))


-- quickCheckWith stdArgs{maxSuccess=10000} prop_palindrome
-- +++ OK, passed 10000 tests.
-- We did 10000 tests because we wanted to be sure that quickCheck generates cases where both x and y are palindromes.
-- This works as we thought.





-- FUNCTIONS THAT WE TESTED --

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f []     = ([], [])
partition f (x:xs) = let
                        (trues, falses) = partition f xs
                     in if (f x) then ((x:trues), falses) else (trues, (x:falses))
                     
deleteLast :: Eq a => a -> [a] -> [a]
deleteLast _ [] = []
deleteLast x (y:ys) = 
    if (vika (y:ys) == x) then (alkulista y ys) else ((deleteLast x (alkulista y ys)) ++ [vika (y:ys)])
-- 
    where
       vika (x:xs) = -- return the last element
            if (length xs > 0) then (vika xs) else x
       alkulista _ []     = []  -- return list without the last element
       alkulista y (z:zs) = y : alkulista z zs
       
deal :: [a] -> ([a],[a])
deal [] = ([],[])
deal (x:xs) = let (ekat,tokat) = deal xs in (tokat,x:ekat)

merge :: Ord a => [a] -> [a] -> [a]
merge [] lista = lista
merge (x:xs) lista = insert x (merge xs lista)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lista = let (alku,loppu) = deal lista in merge (mergeSort alku) (mergeSort loppu)

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) = 
    if (x /= y) then y:(delete x ys) else ys

takeEvens :: [Int] -> [Int]
takeEvens [] = []
takeEvens (x:xs) =
    if even x then x:(takeEvens xs) else (takeEvens xs)
    

(+++) :: [a] -> [a] -> [a]
-- (+++) xs ys = xs ++ ys
(+++) [] [] = []
(+++) [] ys = ys
(+++) xs [] = xs
(+++) (x:xs) ys = x:((+++) xs ys)

myConcat :: [[a]] -> [a]
-- examples: myConcat [[],[],[]] = []
--           myConcat [[2,3,4]] = [2,3,4]
--           myConcat [[1,2],[3,4,5],[6]] = [1,2,3,4,5,6]
myConcat [] = []
myConcat [xs] = xs
myConcat (xs:xss) = xs ++ (myConcat xss)

destutter :: Eq a => [a] -> [a]
destutter [] = []
destutter (x:xs)
            | (xs == []) = [x]
            | (x == head xs) = destutter xs
            | otherwise = x:(destutter xs)

encode :: String -> [(Char,Word8)]  -- You can convert an Int to Word8 with 
                                    -- function fromIntegral. 
encode "" = []
encode str = let
                grouped = group str
                firstElements = head grouped
             in (((head firstElements), (fromIntegral (length firstElements))) : (encode (drop (length firstElements) str)))    



decode :: [(Char,Word8)] -> String
decode [] = ""
decode ((x,n):xs) = let
                        str = replicate (fromEnum n) x
                        
                    in str ++ decode xs
                    
prod :: [Integer] -> Integer
prod []     = 1
prod (x:xs) = x * prod xs

isPalindrome :: String -> Bool
isPalindrome word = 
    let
        backwards = reverse word
    in backwards == word
