-- Tässä tiedostossa on
-- sekä addspice1 että addspice2
module Main where
import Prelude hiding (pure, (<$>))

main :: IO ()
main = do
  putStrLn "hello world"



double    x = (["Double"],x*2)
increment x = (["Incremented by 1"],x + 1)
pure      x = ([],x)

experiment1 = let 
    (l1,step1) = pure 5
    (l2,step2) = double step1
    (l3,step3) = double step2
    (l4,step4) = double step3
    (l5,step5) = increment step4
    (l6,step6) = increment step5
  in (l1++l2++l3++l4++l5++l6,step6)

-- Tässä rakennettu (<$>) operaattori sopivalla tavalla
(<$>) :: (t -> ([a], t2)) -> ([a], t) -> ([a], t2)
f <$> (xs,y) = let
      ( zs, z ) = f y
      in
        (xs ++ zs, z)
infixr 0 <$>


experiment2 =  increment <$> increment <$> 
                  double <$> double <$> double <$> pure 5 

-- Addspice 2 alkaa:

-- Määritellään uusi yhdistetty funktio:
(<=<) :: (t1 -> ([a], t3)) -> (t -> ([a], t1)) -> t -> ([a], t3)
f <=< g = \x -> 
  let
    ( ys, y ) = g x
    ( zs , z ) = f y 
  in
    (ys ++ zs , z)

experiment3 = (increment <=< increment <=< double <=< double <=< double 
          <=< pure ) 5