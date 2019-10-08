
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
  asMeters luku = M luku

  fromMeters :: Meter -> Double
  fromMeters (M luku) = luku

  asLength :: Int -> Length
  asLength luku = luku

  mkVector :: Int -> Int -> Vector2 Int
  mkVector l1 l2 = V2 l1 l2 -- HUOM tässä joku vika

  combine :: Vector2 Int -> Maybe (Vector2 Int) -> OneOrTwo Int Bool
  combine vektori ehka = case ehka of
          Nothing -> This 1
          Just x -> These 2 True

  combine3 :: Vector2 Int -> Maybe Bool -> Maybe String
               -> OneOrTwo Int (OneOrTwo Bool String)
  combine3 vektori eLuku eJono = case (eLuku, eJono) of
            (Nothing, Nothing) -> This 1
            (Nothing, Just x) -> These 2 (This True)
            (Just x, Nothing) -> These 2 (This True)
            (Just x, Just y) -> These 3 (These True "asia")

  submitDay :: Submission -> Int
  submitDay (S _ _ (d,_,_)) = d


  getOther :: OneOrTwo a b -> Maybe b
  getOther juttu = case juttu of
           (This a) -> Nothing
           (That b) -> Just b
           (These a b) -> Just b
