{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- Tässä vähän epäselviä kohtia vielä
-- Mayben käyttöön liittyen..

f1 :: (Int,Char,Bool) -> Char
f1 (a,b,c) = b 

f2 :: (a,b,c) ->  b 
f2 (a,b,c) = b

f3 :: (a,(b,c,d),e) -> c
f3 (a,(b,c,d),e) = c 

f4 :: [a] -> Maybe a
f4 s = case s of
    [] -> Nothing
    x:xs -> Just x

f5 :: Either Int String -> Maybe String
f5 a = case a of
    Left _ -> Nothing
    Right c -> Just c

f6 :: Either a b -> Maybe b
f6 a = case a of
    Left _ ->  Nothing
    Right b -> Just b

g1 :: Maybe a -> b -> Maybe (Either a b)
g1 a b = case a of
    Nothing -> Just (Right b)
    Just c -> Just (Left c)

g2 :: a -> b ->  (a,b)
g2 a b = (a,b)

g3 :: a -> b -> (Either a b)
g3 a b = Left a