module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid --hiding (Sum)

main :: IO ()
main = undefined

newtype MonMap k v = Mon (Map k v) deriving (Eq,Show,Ord)



instance (Ord k,Monoid m) => Monoid (MonMap k m) where
    mempty = Mon (Map.empty)

instance (Ord k,Semigroup m) => Semigroup (MonMap k m) where
    Mon a <> Mon b = Mon (Map.unionWith (<>) a b)

instance Functor (MonMap k) where
    fmap f (Mon k) = Mon (fmap f k)

mon :: k -> v -> MonMap k v
mon key value = Mon (Map.singleton key value)

-- Read through the MonMap example above and add few more analysis functions:

data EVT = PowerOn | PowerOff deriving (Eq,Show)



data EventRecord 
  = Event {systemID::String, event :: EVT} 
    deriving (Eq,Show)
-- A 2 , B 2, C 1
exampleEvents 
  = [Event "A" PowerOn
    ,Event "B" PowerOff
    ,Event "B" PowerOn
    ,Event "A" PowerOff
    ,Event "B" PowerOff
    ,Event "C" PowerOff
    ,Event "B" PowerOn
    ,Event "B" PowerOn
    ,Event "A" PowerOn
    ,Event "B" PowerOff
    ,Event "C" PowerOn
    ,Event "C" PowerOff
    ,Event "A" PowerOff
    ]




machineStates :: [EventRecord] -> MonMap String (Last EVT)
machineStates = foldMap (\x ->  mon (systemID x) 
                        (Last (Just (event x)))) 

-- Nämä lisätty!
powerOns :: [EventRecord] -> MonMap String (Sum Int)
powerOns x = foldMap (\x -> if (event x) == PowerOn then 
            mon (systemID x) (Sum 1) else mon (systemID x) (Sum 0 )) x
--which counts how many times each of the machines have turned on.
firstOn :: [EventRecord] -> First String 
firstOn x = foldMap (\x -> if (event x) == PowerOn then 
    First (Just (systemID x)) else First Nothing) x


-- Monoidi cycles-tehtävän ratkaisemiseksi:
-- Olennaisesti tässä tehdään
-- eventeistä lista, josta poistetaan
-- samanlaiset peräkkäiset eventit.
-- Otettu käytännössä suoraan
-- Ville Tirrosen luentoslideilta
-- partition-esimerkin koodi, jossa
-- poistetaan merkkijonosta peräkkäiset
-- samat kirjaimet. 
data DS a = DS a [a] a | Empty deriving (Eq,Show)

instance (Eq a) => Monoid (DS a) where
    mempty = Empty

instance Eq a => Semigroup (DS a) where
    (DS x xs y) <> (DS z zs h)
        = DS x (xs ++ zsu) h
        where 
            zsu =  if y == z then tail zs else zs

-- Laske kuinka monta kertaa
-- menee päälle ja sen jälkeen pois
cycles :: [EventRecord] -> MonMap String (Sum Int)
cycles = fmap g . (foldMap (\x -> mon (systemID x) (DS (event x) [event x] (event x))))
            where
                g (DS x xs y) = if x == PowerOff 
                                then Sum (((length xs)-1) `div` 2)
                                    else Sum ((length xs) `div` 2)
           

