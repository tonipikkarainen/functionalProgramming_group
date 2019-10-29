module Main where

type Queen = (Int, Int) -- (x,y)
type Setup = [Int]           -- y:t

-- millä tavoin kuningattaret voivat olla yhdessä sarakkeessa
vaihtoehdot :: [Setup]
vaihtoehdot = [[1], [2], [3], [4], [5], [6], [7], [8]]

yhdista :: Setup -> Setup -> Setup
yhdista s1 s2 = s1++s2

yhdistaMonta :: [Setup] -> [Setup] -> [Setup]
yhdistaMonta xs ys = [yhdista x y | x<-xs, y<-ys, onkoOk(yhdista x y)]

onkoOk :: Setup -> Bool
onkoOk ratk = and [not (uhkaako q1 q2)
               | q1<-coords
                , q2<-coords]
               where coords = zip [1..] ratk

-- palauttaa false jos kuningattaret ei uhkaa toisiaan, true jos uhkaa
uhkaako :: Queen -> Queen -> Bool
uhkaako (x1,y1) (x2,y2)
   | (x1,y1) == (x2,y2) = False
   | y1 == y2 =  True
   | x1 == x2 = True
   | (x1-y1) == (x2-y2) = True
   | (x1+y1) == (x2+y2) = True
   | otherwise = False

-- itse funktio basecase + rekursio
laskeKun :: Int -> [Setup]
laskeKun 1 = vaihtoehdot
laskeKun x = let
            puolet = laskeKun (x `div` 2)
          in yhdistaMonta puolet puolet

main :: IO ()
main = do
  putStrLn "hello world"
