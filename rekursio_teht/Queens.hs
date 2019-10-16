module Exercise where
import Data.List

-- Kuvaa monta saraketta setupissa (length) ja millä rivillä kuningatar on
type Setup = [Int]

-- Tätä kutsumalla saadaan ratkaisut, tällä idealla toimii
-- optimaalisesti, kun tätä kutsuu "sol 8" 
-- Antaa 92 eri vaihtoehtoa kuningattarien paikoille
sol :: Int -> [Setup]
sol 1 = [[1],[2],[3],[4],[5],[6],[7],[8]]
sol x = 
    yhdista (sol (x `div`2)) (sol (x `div` 2))

-- Yhdistää uuden listan listoista, joissa sisemmät listat
-- ovat yhdistetty argumenttien sisemmistä listoista.
-- Lisäksi tarkistetaan hyväksytäänkö yhdistetty lista
-- eli uhkaako siellä yksikään kuningatar toista kuningatarta.
-- Tässä tosin turhaan käydään läpi jo testattuja vaihtoehtoja
-- Esim. jos yhdistetään [1,3,6,8] ja [2,4,1,6] niin eka listakin käydään
-- läpi vaikka se on jo testattu...
yhdista:: [Setup] -> [Setup] -> [Setup]
yhdista xs ys = [ x++y | x <- xs, y<-ys, not (tutkiLista (luoTuple(x++y))) ]  

-- Uhkaako kaksi kuningatarta toisiaan
threatens :: (Int,Int) -> (Int,Int) -> Bool
threatens (a1,a2) (b1,b2)
   | (a1,a2) == (b1,b2) = False -- Queen doesn't threaten herself
   | a2 == b2           = True  -- On the same row
   | a1 == b1           = True  -- On the same column
   | (a1-a2) == (b1-b2) = True  -- diagonal
   | (a1+a2) == (b1+b2) = True  -- diagonal
   | otherwise = False


-- palauttaa listan ("sarake","rivi")
-- tein tämän jotta pystyin käyttämään threatens funktiota.
luoTuple :: Setup -> [(Int,Int)]
luoTuple [] = []
luoTuple (x:xs) = 
    let 
        takaperin = reverse (x:xs)
        (y:ys) = takaperin 
    in 
        ((length takaperin),y):(luoTuple (reverse ys) )
-- Tämä hyödyntää "tutkiUhkaako" funktiota, jolla tämä antaa yhden tuplen ja 
-- tuplelistan, joka käydään läpi.
tutkiLista:: [(Int,Int)] -> Bool
tutkiLista [] =False 
tutkiLista (x:xs) 
    | tutkiUhkaako x xs = True
    | otherwise = tutkiLista xs

-- Tämä hyödyntää threatens-funktiota, joka testaa aina ensimmäistä parametria ja 
-- listan ensimmäistä parametria (uhkaavatko ne toisiaan).
tutkiUhkaako:: (Int,Int) -> [(Int,Int)] -> Bool
tutkiUhkaako _ [] = False
tutkiUhkaako y (x:xs) 
    | (threatens y x) = True
    | otherwise = tutkiUhkaako y xs    




