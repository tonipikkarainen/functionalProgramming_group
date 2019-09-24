newtype Meter = M Double 
    deriving (Eq,Ord,Show)

type Length = Int

data Vector2 a = V2 a a 
    deriving (Eq,Show)

data OneOrTwo a b = This a | That b | These a b
    deriving (Eq,Show)

data Submission 
  = S {student :: String, content :: String, date :: (Int,Int,Int)}
    deriving (Eq,Show)


asMeters :: Double -> Meter
asMeters a = M a 

fromMeters :: Meter -> Double
fromMeters (M a) = a 

asLength :: Int -> Length
asLength a = a

mkVector :: Int -> Int -> Vector2 Int
mkVector a b = (V2 a b) 

combine :: Vector2 Int -> Maybe (Vector2 Int) -> OneOrTwo Int Bool
combine a b =  case b of
    Nothing -> This 1
    Just (V2 _ _) -> These 3 True

combine3 :: Vector2 Int -> Maybe Bool -> Maybe String 
             -> OneOrTwo Int (OneOrTwo Bool String)
combine3 a b c = case b of
    Nothing -> case c of 
        Nothing -> These 1 (These True "5")
        Just c -> That (These False "3")
    Just b -> case c of 
        Nothing -> This 55
        Just c -> This 66

submitDay :: Submission -> Int
submitDay (S _ _ (a,_,_)) = a 

getOther :: OneOrTwo a b -> Maybe b 
getOther a = case a of
    This _ -> Nothing
    That b -> Just b
    These _ b -> Just b  