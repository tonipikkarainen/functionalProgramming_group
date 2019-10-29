import Data.Time.Clock

data Season = Winter | Spring | Summer | Autumn deriving Show
data Month = Jan | Feb | Mar | Apr | May | Jun
            | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq, Ord)

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

data Event = MayDay | IndependenceDay | MothersDay

instance Seasonal Event where
  season MayDay = Spring
  season IndependenceDay = Winter
  season MothersDay = Spring


--data UTCTime = Day
-- missä Day on päivännumero ja aika keskiyöstä sekunteina.
-- 1858-11-17 on päivä numero 0 ja vuorokaudessa 86401 s

instance Seasonal UTCTime where
    (UTCTime day time)
     | day 'mod' 365 < 14 = Autumn
     | day 'mod' 365 < 91 = Winter
     | day 'mod' 365 < 183 = Spring
     | day 'mod' 365 < 275 = Summer
     | day 'mod' 365 < 365 = Autumn
