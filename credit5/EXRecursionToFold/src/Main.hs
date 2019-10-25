{-#LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where



main :: IO ()
main = do
  putStrLn "hello world"



foldl' f z []     = z
foldl' f (!z) (x:xs) = foldl f (f z x) xs

--length :: [a] -> Int
--concat :: [[a]] -> [a]
--remove :: Eq a => a -> [a] -> [a] (remove only the first instance of the value from the list)
--find :: (a->Bool) -> [a] -> Maybe a
--filter :: (a->Bool) -> [a] -> [a]
--take :: Int -> [a] -> [a]
--nub :: Eq a => [a] -> [a] (removes all duplicates from a list)


-- Pitää laskea kaikki, joten ei tarvitse olla laiska
-- ei pidä toimia äärettömille listoille
length' :: [a] -> Int
length' xs = foldl' (\x _ -> x+1) 0 xs

-- Pitää laskea kaikki, joten ei tarvitse olla laiska
-- ei pidä toimia äärettömille listoille
concat' :: [[a]] -> [a]
concat' xs = foldl' (\x a -> a ++ x) [] xs

-- Tässä vähän kummallinen toteutus, kun 
-- g:llä pitää vielä tehdä reverse, mutta
-- siinä foldl' ja se toimii!
--remove :: Eq a => a -> [a] -> [a]
--remove pois xs = g (foldl' (\(t,ys) a -> if a == pois && not t  
--           then (True,ys) 
--            else (t,a:ys)) (False,[]) xs) where
--              g (x,y) = reverse y 

-- Tässä nyt fiksummin tehty!!
-- Tuo \x -> [] on nyt se empty!!!
remove :: Eq a => a -> [a] -> [a]
remove n xs = foldr op (\x y -> []) xs False n
  where
    op x poista1 =  -- tässä poista1 on nyt "acc"!!
      \n pois-> 
        if x == pois && not n
          then  poista1 True pois
          else x:poista1 n pois

-- Tässä käydään koko lista läpi, mutta olisiko pakko?
-- Jos löydetään, voitaisiinko keskeyttää foldi?
find :: (a->Bool) -> [a] -> Maybe a
find g xs = foldl' (\x a -> if g a  then (Just a) else x) Nothing xs

-- Tässä kohtaa käytetty foldr:ää 
-- jotta saataisiin lista samassa järjestyksessä
-- tällä toteutuksella. Ei siksi, että tarvitsisi olla
-- laiska. Onko väärin? Miten tuon järjestyksen saamisen
-- samaksi toteuttaisi foldl':llä ilman reverseä..?
filter' :: (a->Bool) -> [a] -> [a]
filter' g xs = foldr (\a x -> if g a then a:x else x ) [] xs

-- Tässä voi olla laiska
-- sopi äärettömille listoille
--
take' :: Int -> [a] -> [a]
take' y xs = aputake xs y

-- Tehdään funktio, joka palauttaa funktion, joka palauttaa tietyn määrän
-- alkioita
-- Eli pitää ajatella, että "tyhjän lista tapaus" on tässä tuo
-- (\x -> [])  ja se pitää olla samaa tyyppiä mitä halutaan palauttaa.
-- Ja muista että op:n pitää myös palauttaa samaa tyyppiä,
-- mitä halutaan lopulta palauttaa!
-- ja foldr:ssä op saa listan alkion aina ensimmäiseksi parametriksi
-- ja "loppulistan - eli tämä on lopulta se "empty" " 
-- toiseksi parametriksi
aputake :: [a] -> (Int -> [a])
aputake = foldr op (\x -> []) 
  where
  op alkio funktio = 
    \luku ->
      if luku > 0 
        then alkio: funktio (luku-1) -- funktio (luku - 1 ) 
        else []
        

-- funktio (luku - 1 ) edustaa nyt koko osuutta:
-- foldr f z xs (luku - 1) !!

--foldr f z []     = z
--foldr f z (x:xs) = x `f` foldr f z xs
-- 
-- aputake [1,2,3,4] 2
-- = (foldr op (\x -> []) [1,2,3,4]) 2
-- = 1 : ((foldr op (\x -> []) [1,2,3,4]) 1)
-- = 1 : (2: ((foldr op (\x -> []) [1,2,3,4]) 0))
-- = 1 : (2: [])
-- 
-- aputake [1,2] 2
-- = (foldr op (\x -> []) [1,2]) 2
-- = 1 :  (foldr op (\x -> []) [2]) 1
-- = 1 : (2: (foldr op (\x -> []) []) 0)
-- = 1 : (2: ([])

nub :: Eq a => [a] -> [a] --(removes all duplicates from a list)
nub = foldr (\alkio x -> if elem alkio x then x else alkio:x) [] 

