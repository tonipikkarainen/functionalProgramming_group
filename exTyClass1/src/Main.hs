module Main where
import Data.Time.Clock

data Season = Winter | Spring | Summer | Autumn 
    deriving Show

data Month  = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec 
    deriving (Show,Eq,Ord)

data Event = MayDay | IndependenceDay | MothersDay
    deriving Show
    
class Seasonal a where
    season :: a -> Season

instance Seasonal Month where
    season Jan = Winter
    season Feb = Winter
    season Mar = Spring
    season Apr = Spring
    season May = Spring
    season Jun = Summer
    season Jul = Summer
    season Aug = Summer
    season Sep = Autumn
    season Oct = Autumn
    season Nov = Autumn
    season Dec = Winter
    
instance Seasonal Event where
    season MayDay = Spring
    season IndependenceDay = Winter
    season MothersDay = Spring
    
instance Seasonal UTCTime where
    season (UTCTime day time) 
        | (d `rem` 365) < 92 = Winter
        | (d `rem` 365) >= 92 && (d `rem` 365) < 171 = Spring
        | (d `rem` 365) >= 171 && (d `rem` 365) < 263 = Summer
        | (d `rem` 365) >= 263 = Autumn
            where
            d = fromEnum day
-- Here winter starts on Nov 17th since the day number one in the Day datatype
-- is 1858-11-17. For simplicity, I assumed that every year lasts 365 days, which
-- is not the case, but I suppose it is enough for this excercise.

main :: IO ()
main = do
  putStrLn "hello world"
