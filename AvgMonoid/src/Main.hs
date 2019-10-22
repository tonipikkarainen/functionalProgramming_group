module Main where
import Data.Monoid 

newtype Average a b = Average (a, b) deriving (Show) 

instance (Num a, Num b) => Semigroup (Average a b) where
    Average (x,y) <> Average (z,k) = Average (x+z, y+k)
    
instance (Num a, Num b) => Monoid (Average a b) where
    mempty = Average (0,0)
   
lstToAve :: [Double] -> Average (Sum Int) (Sum Double)
lstToAve lst = Average (foldMap (\x -> (Sum 1, Sum x)) lst)

aveToDouble :: Average (Sum Int) (Sum Double) -> Double
aveToDouble (Average (Sum count, Sum sum)) = sum / fromIntegral (count)

averageOfList :: [Double] -> Double
averageOfList lst = aveToDouble (lstToAve lst)

main :: IO ()
main = do
  putStrLn "hello world"
