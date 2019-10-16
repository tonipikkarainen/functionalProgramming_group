module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Picture
import System.Random


data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo -- Ufo added here
                   | GameOver 
                   deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float
type Health       = Int

class Spatial a where
    location :: a -> PointInSpace
    size :: a -> Float


-- (Spatial a, Spatial b) => a -> b -> Bool

data Ship   = Ship   PointInSpace Velocity      
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity 
    deriving (Eq,Show)

data Ufo  = Hunting PointInSpace Velocity Health| Fleeing PointInSpace Velocity Health|
    Exploding PointInSpace | Waiting PointInSpace Health Age
    deriving (Eq,Show)

instance Spatial Rock where
  location (Rock l _ _) = l
  size (Rock _ s _ ) = s 

instance Spatial Ship where
  location (Ship l _ ) = l
  size (Ship _ _)  = 10

instance Spatial Ufo where
  location x = case x of
      Hunting l _ _  -> l
      Fleeing l _ _  ->  l
      Exploding l -> l
      Waiting l _ _  -> l
  size x  = ufoSize

instance Spatial Bullet where
  location (Bullet l _ _ ) = l
  size x  = 10

-- SOMETHING ADDED!
ufoXpaikka = -50
ufoSize = 30
ufoAlkuNopeus = 1
ufoElamat = 4
initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)    
                   ,Rock (-45,201)  45 (13,-8) 
                   ,Rock (45,22)    25 (-2,8)  
                   ,Rock (-210,-15) 30 (-2,-8) 
                   ,Rock (-45,-201) 25 (8,2)   
                   ] -- The default rocks
                   (Ship (0,0) (0,5)) -- The initial ship
                   [] -- The initial bullets (none)
                   (Hunting (ufoXpaikka,-50) (0,ufoAlkuNopeus) ufoElamat) -- The Ufo (added)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver   = GameOver  

-- Ufolla on neljä tilaa. Ufo siirtyy updateUfo funktion avulla tilasta toiseen tiettyjen 
-- sääntöjen mukaisesti.
-- Aloitetaan hunting tilasta, jossa ufo liikkuu kohti laivaa.
--  Jos ufo ammutaan se siirtyy fleeing tilaan, jossa ufo saa nopeuden pois päin laivasta.
--  Jos t (health) on pienempää kuin 1 mennään exploding tilaan
--  Muuten pysytään hunting tilassa
-- Fleeing:
--  Jos fleeing tilassa joudutaan ammutuksi, mennään waiting tilaan, jossa ufo jähmettyy
--  paikalleen.
--  Jos taas fleeing tilassa health on pienempää kuin 3 mennään hunting tilaan (eli pienillä 
--  energioilla ollaan aina hunting tilassa)
--  muuten pysytään fleeing tilassa 
-- waiting
--   tässä tilassa ufo on tietty ikä (age). eli ollaan paikallaan waiting tilassa niin kauan
--   kun ikä on pienempää kuin joku tietty arvo. Tai jos ufoa ammutaan niin mennään 
--   fleeing tilaan (tai jos energia liian pieni, mennään exploding tilaan).
-- Exploding
--   Tässä ufo "räjähtää" ja jää ruudulle paikalleen punaisena ympyränä. Tästä tilasta
--    ei enää päästä mihinkään muuhun
-- 
-- collisions - lisätty
-- Jos kivi törmää ufoon sen nopeus kaksinkertaistuu
-- Kun luoti osuu laivaan se saa lisää elinaikaa.
-- nyt on törmäykset : 
-- ufo - kivi, kivi - laiva, laiva -  ufo, ufo - bullet,
-- bullet - laiva, bullet-kivi
-- eli pitäisi olla kaikki törmäykset
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets ufo1) -- Ufo added here
  | any (collides (Ship shipPos shipV)) rocks  = GameOver 
  | collides (Ship shipPos shipV) ufo1  = GameOver 
  | otherwise = Play (concatMap (updateRock ufo1) rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              (updateUfo ufo1) -- Ufo added here
  where
      -- Nämä funktiot luotu Spatial tyyppiluokan avulla:
      collides :: (Spatial a, Spatial b) => a -> b -> Bool
      collides x y =  
        magV ( location x .- location y ) < size y

      collidesWithB :: Spatial a => a -> Bool
      collidesWithB r 
       = any (\b -> collides b r) bullets 
      
      -- Nämä siis on korvattu
      {-
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _)  = 
        magV (rp .- p) < s

      
      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 


      -- Tämän toteutus Either Rock Ufo parametrilla??
      --add
      collidesWithUfo :: PointInSpace -> Ufo -> Bool
      collidesWithUfo p ufo
       = case ufo of 
        Hunting x _ _ -> magV (x .- p) < ufoSize
        Fleeing  x _ _ -> magV (x .- p) < ufoSize
        Waiting  x _ _ -> magV (x .- p) < ufoSize
        Exploding _ -> False
      --add
      collidesWithBulletUfo :: Ufo -> Bool
      collidesWithBulletUfo r 
       = any (\(Bullet bp _ _) -> collidesWithUfo bp r) bullets
     -}
      updateRock :: Ufo -> Rock -> [Rock]
      updateRock u r@(Rock p s v)
       | collidesWithB r && s < 7 
            = []
       | collidesWithB r && s > 7 
            = splitRock r
       | (collides r u)  = [Rock (restoreToScreen (p .+ timeStep .* (2*v))) s (2*v)]
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
      
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collides (Bullet p v a)) rocks 
             = [] 
        | collides (Bullet p v a) ufo1 = [] 
        | collides (Bullet p v a) (Ship shipPos shipV) = 
                    [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a - timeStep)]      
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 
      
      --add
      updateUfo :: Ufo -> Ufo 
      updateUfo ufosum = case ufosum of   
        Hunting pos v t 
            | collidesWithB ufosum ->Fleeing (restoreToScreen (pos .+ timeStep .* v)) 
             (magV(shipV) .* norm(pos .- shipPos)) (t-1) --Tässä ufo lähtee karkuun kun saa osuman
            | t < 1 ->Exploding pos 
            | otherwise  -> Hunting (restoreToScreen (pos .+ timeStep .* v))
             (magV(shipV).*norm(shipPos .- pos)) (t)
        Fleeing pos v t
            |collidesWithB ufosum ->Waiting (restoreToScreen (pos .+ timeStep .* v)) 
              (t-1) 0
            | t < 1 ->Exploding pos
            | t < 3 -> Hunting (restoreToScreen (pos .+ timeStep .* v)) (magV(shipV).*norm(shipPos .- pos)) (t)
            | otherwise -> Fleeing (restoreToScreen (pos .+ timeStep .* v)) 
             v (t)
        Waiting pos t a
            | a > 100 -> Hunting (restoreToScreen (pos)) (magV(shipV).*norm(shipPos .- pos)) (t)
            | collidesWithB ufosum ->Fleeing (restoreToScreen (pos)) 
             (magV(shipV).*norm(pos .- shipPos)) (t-1)
            | t < 1 ->Exploding pos 
            | otherwise-> Waiting (restoreToScreen (pos) )
              (t) (a+1) 
        Exploding (xu,yu) -> Exploding (xu,yu)

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-400) = 800+x
    | x > 400    = x-800
    | otherwise  = x

drawWorld :: AsteroidWorld -> Picture

drawWorld GameOver 
   = scale 0.3 0.3 
     . translate (-400) 0 
     . color red 
     . text 
     $ "Game Over!"

-- SOMETHING ADDED!
drawWorld (Play rocks (Ship (x,y) (vx,vy))  bullets ufotype)
  = pictures [ship, asteroids,shots, ufo]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [translate x y (color orange (rectangleWire s s)) -- rocks changed 
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color blue (rectangleSolid 10 10)) -- Changed bullets to blue rectangles
                         | Bullet (x,y) _ _ <- bullets]
    ufo       = case ufotype of
      Hunting (xu,yu) _ _ -> color yellow (pictures [translate xu yu (rectangleSolid ufoSize 7)])
      Fleeing (xu,yu) _ _ -> color blue (pictures [translate xu yu (rectangleSolid ufoSize 7)])
      Waiting (xu,yu) _ _ -> color green (pictures [translate xu yu (rectangleSolid ufoSize 7)])
      Exploding (xu,yu) -> color red (pictures [translate xu yu (circle 30)])
handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

-- SOMETHING ADDED!
-- Now it is possible to start a new game after "game over" with space
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) GameOver = initialWorld 
                        


handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) bullets ufo)
             = Play rocks (Ship shipPos newVel) 
                          (newBullet : bullets)
                          ufo
 where 
     newBullet = Bullet shipPos 
                        (negate 150 .* norm (shipPos .- clickPos)) 
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

handleEvents _ w = w


type PointInSpace = (Float, Float)
(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)

infixl 6 .- , .+
infixl 7 .*

norm :: PointInSpace -> PointInSpace
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: PointInSpace -> Float
magV (x,y) = sqrt (x**2 + y**2) 

limitMag :: Float -> PointInSpace -> PointInSpace
limitMag n pt = if (magV pt > n) 
                  then n .* (norm pt)
                  else pt

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)


main = play 
         (InWindow "Asteroids!" (550,550) (20,20)) 
         black 
         24 
         initialWorld 
         drawWorld 
         handleEvents
         simulateWorld


        
