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

exampleEvents 
  = [Event "A" PowerOff
    ,Event "B" PowerOff
    ,Event "B" PowerOff
    ,Event "A" PowerOff
    ,Event "B" PowerOff
    ,Event "C" PowerOff
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
--which finds out which machine was turned on first.
--In both cases, your answer should have the form foldMap f 
--where f inserts the appropriate elements to your monoid.