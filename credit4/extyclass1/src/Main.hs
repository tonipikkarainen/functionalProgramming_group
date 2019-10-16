module Main where
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time

main :: IO ()
main = undefined
data Season = Winter | Spring | Summer | Autumn deriving Show
-- Lisätty "Aug"
data Month  = Jan | Feb | Mar | Apr | May | Jun 
            | Jul |Aug| Sep | Oct | Nov | Dec deriving (Show,Eq,Ord)

data Event = MayDay | IndependenceDay | MothersDay

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

--example :: UTCTime
--example = UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)

-- Tässä instanssi UTCTimelle Seasonalista
-- Katsotaan UTCTimen päivämäärän
-- kuukautta ja annetaan vuoden aika sen mukaan
instance Seasonal UTCTime where
    season (UTCTime x y) 
                | month == 12 || month < 3 = Winter
                | month < 6 = Spring
                | month < 9 = Summer
                | month < 12 = Autumn   
            where 
                (year, month, day) = toGregorian x  