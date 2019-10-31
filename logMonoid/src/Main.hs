module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid

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

data EVT = PowerOn | PowerOff deriving (Eq, Show)

data EventRecord = Event {systemID :: String, event :: EVT}
                   deriving (Eq, Show)

exampleEvents = [Event "A" PowerOn
                 ,Event "B" PowerOn
                 ,Event "B" PowerOn
                 ,Event "B" PowerOff
                 ,Event "A" PowerOff
                 ,Event "B" PowerOn
                 ,Event "C" PowerOn ]

machineStates :: [EventRecord] -> MonMap String (Last EVT)
machineStates = foldMap (\x -> mon (systemID x)
                        (Last (Just (event x))))

-- Uudet funktiot:
-- montako kertaa mikäkin masiina on käännetty päälle
powerOns :: [EventRecord] -> MonMap String (Sum Int)
powerOns = foldMap (\x -> if (event x) == PowerOn
                    then mon (systemID x) (Sum 1)
                    else mon (systemID x) (Sum 0))


-- mikä masiina on käynnistetty ekana
firstOn :: [EventRecord] -> First String
firstOn = foldMap (\x -> if (event x) == PowerOn
                    then (First (Just (systemID x)))
                    else (First Nothing) )

--- exCyclesMonoid

data Events a = Events a [a] a | Empty deriving (Eq, Show)

instance Eq a => Monoid (Events a) where
  mempty = Empty

instance Eq a => Semigroup (Events a) where
  (Events x xs y) <> (Events z zs w) =
    if y==z then Events x (xs ++ tail zs) w
            else Events x (xs ++ zs) w
  --- tarkistetaan siis ovatko ensimmäinen ja viimeinen samat ja jos ovat,
  -- hävitetään toinen

cycles :: [EventRecord] -> MonMap String (Sum Int)
cycles = fmap g . foldMap f
    where
      f :: EventRecord -> MonMap String (Events EVT)
      f = \x -> mon (systemID x) (Events (event x) [event x] (event x))
      g :: Events EVT -> Sum Int
      g (Events x xs y) = if x == PowerOff
                             then Sum ((length xs - 1) `div` 2)
                             else Sum ((length xs) `div` 2)
