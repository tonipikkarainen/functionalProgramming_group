import Data.Monoid

--newtype Sum a = Sum {theSum :: a} deriving (Show, Eq, Ord)

--instance Num a => Semigroup (Sum a) where
  --Sum a <> Sum b = Sum (a+b)

--instance Num a => Monoid (Sum a) where
--  mempty = Sum 0

ka (x,y) = (getSum x) / (getSum y)

average :: [Double] -> Double
average list = ka (foldMap (\x -> (Sum x, Sum 1)) list)
