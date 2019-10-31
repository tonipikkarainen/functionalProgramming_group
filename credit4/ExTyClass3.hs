import Data.Text


class JSON a where
  toJSONString :: a -> String

--instance JSON Int where
  --toJSONString x = show x

instance JSON Integer where
  toJSONString x = show x

instance JSON Char where
  toJSONString a = [a] -- tai show a

instance JSON Text where
  toJSONString t = unpack t

instance (JSON a, JSON b) =>JSON (a,b) where
  toJSONString (a,b) = "[" ++ toJSONString a ++ toJSONString b ++ "]"

instance (JSON a) => JSON [a] where
  toJSONString [] = []
  toJSONString (x:xs) = "[" ++ apu (x:xs) ++"]"
      where
        apu [] = ""
        apu [x] = toJSONString x
        apu (x:xs) = toJSONString x ++ "," ++ apu xs
