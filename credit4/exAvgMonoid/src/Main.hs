module Main where

main :: IO ()
main = undefined

newtype Sum a = Sum {theSum :: a} deriving (Show,Eq,Ord)


instance Num a => Monoid (Sum a) where
     mempty = Sum 0

instance Num a => Semigroup (Sum a) where
     Sum x <> Sum y = Sum (x+y)


averageMon :: [Double] -> Double
averageMon xs = g (foldMap (\x -> (Sum x, Sum 1)) xs) 
                    where
                        g (x,y) = (theSum x)/(theSum y)  
