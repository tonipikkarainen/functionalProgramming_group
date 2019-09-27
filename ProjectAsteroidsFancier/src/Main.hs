module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Picture
import System.Random

-- SOMETHING ADDED!
data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo -- Ufo added here
                   | GameOver 
                   deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float



data Ship   = Ship   PointInSpace Velocity      
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity 
    deriving (Eq,Show)
-- SOMETHING ADDED!
-- Added data Ufo
-- Tähän 3 eri tilaa?
data Ufo  = Ufo  PointInSpace Velocity 
    deriving (Eq,Show)

-- SOMETHING ADDED!
ufoSade = 50
ufoAlkuNopeus = 50
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
                   (Ufo (ufoSade,0) (0,ufoAlkuNopeus)) -- The Ufo (added)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver  

-- SOMETHING ADDED!
-- Mistä timestep tulee, kun tätä kutsutaan ei näy timestep argumenttia???
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets (Ufo ufoPos ufoV)) -- Ufo added here
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              (Ufo newUfoPos newUfoV) -- Ufo added here
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

      -- Ufolle random-nopeus
      -- .+ (cos timeStep , cos timeStep )
      -- v = v + a*timestep
      -- a = v^2/r
      -- Pistetään ufo ympyräradalle
      ufoAcc = -norm ufoPos
      newUfoV = ufoV .+ ((((magV ufoV)**2)/ufoSade)*timeStep .* ufoAcc )

      newUfoPos :: PointInSpace
      newUfoPos = restoreToScreen (ufoPos .+ timeStep .* newUfoV) -- Ufo position added here

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
drawWorld (Play rocks (Ship (x,y) (vx,vy))  bullets (Ufo (xu,yu) (vxu, vxy)) )
  = pictures [ship, asteroids,shots, ufo]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [translate x y (color green (rectangleWire s s)) -- rocks changed 
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color blue (rectangleSolid 10 10)) -- Changed bullets to blue rectangles
                         | Bullet (x,y) _ _ <- bullets]
    ufo       = color green (pictures [translate xu yu (rectangleSolid 30 7)]) -- green ufo added                     

handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

-- SOMETHING ADDED!
-- Now it is possible to start a new game after "game over" with space
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) GameOver = initialWorld 
                        

-- SOMETHING ADDED!
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) bullets (Ufo ufoPos ufoV))
             = Play rocks (Ship shipPos newVel) 
                          (newBullet : bullets)
                          (Ufo ufoPos ufoV)
 where 
     newBullet = Bullet shipPos 
                        (negate 150 .* norm (shipPos .- clickPos)) 
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

-- SOMETHING ADDED!
-- Here ufo is given some velocity to left when keyleft is pressed
{-
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _)
             (Play rocks (Ship shipPos shipVel) bullets (Ufo ufoPos ufoV))
             = Play rocks (Ship shipPos shipVel) 
                          bullets
                          (Ufo ufoPos newUfoV)
 where 
     newUfoV    = ufoV .+ (50 .* (-1,0))

-- SOMETHING ADDED!
-- Here ufo is given some velocity to right when keyright is pressed
handleEvents (EventKey (SpecialKey KeyRight) Down _ _)
             (Play rocks (Ship shipPos shipVel) bullets (Ufo ufoPos ufoV))
             = Play rocks (Ship shipPos shipVel) 
                          bullets
                          (Ufo ufoPos newUfoV)
 where 
     newUfoV    = ufoV .+ (50 .* (1,0))
-}
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


        
