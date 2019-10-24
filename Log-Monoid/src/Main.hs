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
  = [Event "A" PowerOn
    ,Event "B" PowerOn
    ,Event "B" PowerOff
    ,Event "A" PowerOff
    ,Event "B" PowerOn
    ,Event "C" PowerOn
    ,Event "B" PowerOff
    ,Event "A" PowerOn
    ,Event "B" PowerOn
    ,Event "B" PowerOff
    ,Event "A" PowerOff
    ,Event "B" PowerOn
    ,Event "C" PowerOn
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

data EVTlist a = EVTlist a [a] a | Empty deriving (Eq, Show)
 
instance Eq a => Semigroup (EVTlist a) where
    EVTlist x1 xs xLast <> EVTlist y1 ys yLast = 
        if xLast == y1 then EVTlist x1 (xs ++ tail ys) yLast
                       else EVTlist x1 (xs ++ ys) yLast
-- x1 and xLast are the first and last element of xs 
-- i.e. head xs = x1 and last xs = xLast
         
    
instance Eq a => Monoid (EVTlist a) where
    mempty = Empty


cycles :: [EventRecord] -> MonMap String (Sum Int)
cycles = fmap g . foldMap f 
    where
        f :: EventRecord -> MonMap String (EVTlist EVT)
        f = \(Event id state) -> mon id (EVTlist state [state] state)
        g :: EVTlist EVT -> Sum Int
            -- Input is of form 'EVTlist Off [Off, On, Off, On, Off] Off'
            -- or               'EVTlist Off [Off, On, Off, On, Off, On] On'
            -- or               'EVTlist On [On, Off, On, Off, On] On'
            -- or               'EVTlist On [On, Off, On, Off, On, Off] Off'
        g = \(EVTlist x1 xs xLast) -> case (x1, xLast) of
                                        (PowerOff, PowerOff)   -> Sum ((length xs) `div` 2)
                                        (PowerOff, PowerOn)  -> Sum ((length xs) `div` 2 - 1)
                                        (PowerOn, PowerOn) -> Sum ((length xs) `div` 2)
                                        (PowerOn, PowerOff)  -> Sum ((length xs) `div` 2)
        

main :: IO ()
main = do
  putStrLn "hello world"
