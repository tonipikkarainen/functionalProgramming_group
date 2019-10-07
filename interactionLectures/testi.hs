--f:: Num a=>a
--f = 1+1

--g = undefined :: a


--h :: a 

--g ::  [a] -> (a->a) -> [a] 
--g =  (\xs -> \ f -> map f xs)
--
--f:: [a] -> (a->a)
--f = (\xs -> \x ->  head xs) 
----f :: [a] -> (a->a)
----f (a:xs) x = a
----f [] x = x 
--
--kaanna as = case as of
--    [] -> []
--    [x] -> [x]
--    (x:xs) -> (kaanna xs) ++ [x]
--
--j :: Int -> a -> Bool
--j x y = case y of
--    (x:xs) -> True
--    _ ->  False 

tarkista xs = case xs of
    x:(y:(z:(t:(s:ys)))) -> True
    otherwise -> False

f = case Just (Left 1) of
    Nothing        -> "A"  
    Just (Right 1) -> "B" 
    Just x         -> "C" 
    Just (Left 1)  -> "D" 

data PyType = PyBool Bool
            | PyInt Int
            | PyDouble Double
            | PyFun ([PyType] -> IO PyType)