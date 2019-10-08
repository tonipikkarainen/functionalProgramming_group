module Main where


  f1 :: (Int,Char,Bool) -> Char
  f1 (luku, kirj, totuus) = kirj

  f2 :: (a,b,c) -> b
  f2 (a,b,c) = b

  f3 :: (a,(b,c,d),e) -> c
  f3 (a,(b,c,d),e) = c

  f4 :: [a] -> Maybe a
  f4 lista =  case lista of
      [] -> Nothing
      (x:xs) -> Just head lista

  f5 :: Either Int String -> Maybe String
  f5 jotakin = case jotakin of
      Left _ -> Nothing
      Right jotakin -> Just jotakin

  f6 :: Either a b -> Maybe b
  f6 jotakin = case jotakin of
    Left _ -> Nothing
    Right jotakin -> Just jotakin

  g1 :: Maybe a -> b -> (Either a b)
  g1 eka toka = Either (eka toka)

  g2 :: a -> b -> (a,b)
  g2 eka toka = (eka,toka)

  g3 :: a -> b -> (Either a b)
  g3 eka toka = Left eka



main :: IO ()
main = do
  putStrLn "hello world"
