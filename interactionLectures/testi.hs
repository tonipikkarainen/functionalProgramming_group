--f:: Num a=>a
--f = 1+1

--g = undefined :: a


--h :: a 

g ::  [a] -> (a->a) -> [a] 
g =  (\xs -> \ f -> map f xs)

f:: [a] -> (a->a)
f = (\xs -> \x ->  head xs) 
--f :: [a] -> (a->a)
--f (a:xs) x = a
--f [] x = x 

kaanna as = case as of
    [] -> []
    [x] -> [x]
    (x:xs) -> (kaanna xs) ++ [x]