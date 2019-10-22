module Main where
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid

newtype MonMap k v = Mon (Map k v) deriving (Eq,Show,Ord)

instance (Ord k,Monoid m) => Monoid (MonMap k m) where
    mempty = Mon (Map.empty)

instance (Ord k,Semigroup m) => Semigroup (MonMap k m) where
    Mon a <> Mon b = Mon (Map.unionWith (<>) a b)

instance Functor (MonMap k) where
    fmap f (Mon k) = Mon (fmap f k)

mon :: k -> v -> MonMap k v
mon key value = Mon (Map.singleton key value)

data EVT = PowerOn | PowerOff deriving (Eq,Show)

data EventRecord 
  = Event {systemID::String, event :: EVT} 
    deriving (Eq,Show)

exampleEvents 
  = [Event "A" PowerOff
    ,Event "B" PowerOff
    ,Event "B" PowerOn
    ,Event "A" PowerOff
    ,Event "B" PowerOff
    ,Event "C" PowerOff
    ]
   
machineStates :: [EventRecord] -> MonMap String (Last EVT)
machineStates = foldMap (\x ->  mon (systemID x) 
                        (Last (Just (event x))))
    
-- My work starts here --

powerOns :: [EventRecord] -> MonMap String (Sum Int)
powerOns = foldMap f 
    where f = \(Event id event) -> 
            if event == PowerOn 
                then mon id (Sum 1)
                else mon id (Sum 0)

firstOn :: [EventRecord] -> First String
firstOn = foldMap f
    where f = \(Event id event) ->
            if event == PowerOn
                then First (Just id)
                else First Nothing
    
-- CyclesMonoid --

cycles :: [EventRecord] -> MonMap String (Sum Int)
cycles = undefined

main :: IO ()
main = do
  putStrLn "hello world"
