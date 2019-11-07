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
  = [Event "A" PowerOff
    ,Event "A" PowerOn
    ,Event "A" PowerOff
    ,Event "A" PowerOff
    ,Event "A" PowerOn
    ,Event "A" PowerOn
    ,Event "A" PowerOn
    ,Event "A" PowerOff
    ,Event "A" PowerOff
    ,Event "B" PowerOn
    ,Event "B" PowerOn
    ,Event "B" PowerOn
    ,Event "B" PowerOff
    ,Event "B" PowerOff
    ]
-- A - 2
-- B - 2
-- C - 1 


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



--data DS a = DS a [a] a | Empty deriving (Eq,Show)

data Events a = Events a Int a | Empty deriving (Eq, Show)

--instance (Eq a) => Monoid (DS a) where
--    mempty = Empty

instance (Eq a) => Monoid (Events a) where
    mempty = Empty
-- on  0  on   <>   off 0 off -> on 1 off  <> off 1 on -> on 1 on   
--  on 1 off <> on 1 off -> on 2 off  <> off 1 on -> on 3 on  
instance (Eq a) => Semigroup (Events a) where
    Events a x b <> Events c y d
        = Events a xs d--a luku toka
         where 
            xs 
               | b /= c = (x + y + 1)
               | otherwise = (x+y)
               

--instance Eq a => Semigroup (DS a) where
--    (DS x xs y) <> (DS z zs h)
--        = DS x (xs ++ zsu) h
--        where 
--            zsu =  if y == z then tail zs else zs

-- Laske kuinka monta kertaa
-- menee päälle ja sen jälkeen pois
cycles :: [EventRecord] -> MonMap String (Sum Int)
cycles = fmap g . (foldMap (\x -> mon (systemID x) (Events (event x) 0 (event x)))) where
    g (Events eka luku toka) = if eka == PowerOff  
        then Sum ((luku ) `div` 2 ) 
        else Sum ((luku `div` 2)+1) 

{-cycles = fmap g . (foldMap (\x -> mon (systemID x) (DS (event x) [event x] (event x))))
            where
                g (DS x xs y) = if x == PowerOff 
                                then Sum (((length xs)-1) `div` 2)
                                    else Sum ((length xs) `div` 2)-}
           

