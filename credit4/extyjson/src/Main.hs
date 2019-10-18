module Main where
import Data.Text

main :: IO ()
main = undefined

class JSON a where
    toJSONString :: a -> String

-- Ei selviä kaikesta (esim. sisäkkäiset listat), mutta selviää tästä 
-- tapauksesta, mikä vaadittiin:
-- toJSONString ([1,2,3],[(pack "cat",(6,'a')),(pack "dog",(7,'x'))]) ==
-- "[[1,2,3],[[cat,[6,'a']],[dog,[7,'x']]]]" 


instance JSON Integer where
    toJSONString x = show x

instance JSON Int where
    toJSONString x = show x

instance JSON Text where
    toJSONString x = unpack x 

instance JSON Char where
    toJSONString x = show x

instance (JSON a) => JSON [a] where
     toJSONString [] = ""
     toJSONString (x:xs) = "[" ++apu (x:xs) ++"]"
            where 
                apu [] = ""
                apu (x:[]) = toJSONString x
                apu (x:xs) = toJSONString x++ ","++ apu xs
                    
                    

instance (JSON a, JSON b) => JSON (a,b)  where
    toJSONString (x,y) = "["++toJSONString x ++","++ toJSONString y ++"]" 
    