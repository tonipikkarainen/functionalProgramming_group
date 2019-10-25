module Main where
import Data.List
import Data.Ord

main :: IO ()
main = do
  putStrLn "hello world"

freqSort :: String -> String
freqSort xs = 
 let sorted = sort xs
     grouped = group sorted
     byLen = sortBy (comparing length) grouped
     result = concat byLen
 in result

--Convert the above function into point free form. 
--That is, express it using 
--only function composition. 
-- (Do not use let, where or function arguments)

freqSort2 :: String -> String
freqSort2 = concat . sortBy (comparing length) . group . sort


