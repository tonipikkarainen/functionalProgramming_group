class Blub a where
    blub :: a -> String

data Test a  = Maybe a

instance Blub (Maybe a) where
    blub x = case x of 
        Just(_) -> "just"
        Nothing -> "nothing"

instance Blub () where
    blub x = "hep"

data Numero = Numero Int deriving (Show,Eq,Ord)

data Person = Person String deriving (Show)

instance Semigroup Numero where
    (Numero a) <> (Numero b)  = Numero (a*b) 

data Vector a = Vec a a a 


data AllBoolFuns = A (Bool -> Bool)

data ConstBool a = C Bool

data Pred a = P (a -> Bool)

data Select a = S (Bool -> a)

--instance Blub a => [a] where 
--    blub xs = "hep"

--instance HasCapacity Either where ...
--
--instance HasCapacity (Either String) where ...
--
--instance HasCapacity (Either String Int) where ...