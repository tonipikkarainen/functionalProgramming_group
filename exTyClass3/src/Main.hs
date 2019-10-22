module Main where
import Data.Text

class JSON a where
    toJSONString :: a -> String
    
instance JSON Int where
    toJSONString x = show x
    
instance JSON Char where
    toJSONString x = show x
    
instance JSON Text where
    toJSONString txt = "'" ++ unpack txt ++ "'"
    
instance (JSON a, JSON b) => JSON (a,b) where 
    toJSONString (x,y) = "(" ++ toJSONString x ++ "," ++ toJSONString y ++ ")"

instance (JSON a) => JSON [a] where 
    toJSONString [] = []
    toJSONString [x] = toJSONString x ++ "]"
    toJSONString (x:xs) = "[" ++ (toJSONString x) ++ "," ++ (apuri xs)
        where 
        -- Helper function is needed to ensure that there is a square bracket in 
        -- the end of the list.
        apuri [] = ""
        apuri [x] = toJSONString x ++ "]"
        apuri (x:xs) = (toJSONString x) ++ "," ++ (apuri xs)
            

main :: IO ()
main = do
  putStrLn "hello world"
