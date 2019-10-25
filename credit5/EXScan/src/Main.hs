module Main where

main :: IO ()
main = do
  putStrLn "hello world"

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f x [] =  x : [] 
scanr' f x (y:ys) = (f y h) : scanr' f x ys
    where
      (h:hs) = scanr' f x ys
       


scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x [] = x : []
scanl' f x (y:ys) = x : scanl' f (f x y) ys

-- scanl (+) 0 [1,2,3]
-- == {- Should be -}
-- [0,1,3,6]