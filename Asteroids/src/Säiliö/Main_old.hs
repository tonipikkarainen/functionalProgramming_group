-- | Improved Asteroids Game
-- | Authors: Niko Heikkilä, Katri Passi, AP Väisänen

module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

-- Define base game area --
data AsteroidWorld = Play [Rock] Ship UFO [Bullet]
                   | GameOver
                   | Win
                   deriving (Eq,Show)

type Velocity     = (Float, Float)  -- Velocity is a pair with two floats
type Size         = Float           -- Size is Float
type Age          = Float           -- Age is Float
type Health       = Int             -- Health is Int
type FleeTime     = Float           -- FleeTime is Float

-- Define Ship, Bullet and Rock --
data Ship   = Ship   PointInSpace Velocity
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity
    deriving (Eq,Show)
-- Our UFO --
data UFO    = UFO  PointInSpace Velocity Health UFOstate FleeTime
    deriving (Eq, Show)
-- Data type for the state of the UFO --
data UFOstate = Hunting
              | Fleeing
              | Exploding
    deriving (Eq, Show)

initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)
                   ,Rock (-45,201)  45 (13,-8)
                   ,Rock (45,22)    25 (-2,8)
                   ,Rock (-210,-15) 30 (-2,-8)
                   ,Rock (-45,-201) 25 (8,2)
                   ] -- The default rocks
                   (Ship (0,0) (0,0)) -- The initial ship
                   (UFO  (75, 75) (2, 5) 3 Hunting 0) -- The initial UFO (health=3)
                   [] -- The initial bullets (none)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver
simulateWorld _        Win               = Win

simulateWorld timeStep (Play rocks (Ship shipPos shipV) (UFO ufoPos ufoV health state ft) bullets)
  | any (collidesWith shipPos) rocks = GameOver
  | (collidesWithUFO shipPos) (UFO ufoPos ufoV health state ft) = GameOver
  | health==0 = Win
  | otherwise = Play (concatMap updateRock rocks)
                              (Ship newShipPos shipV)
                              (updateUFO (UFO ufoPos ufoV health state ft))
                              (concat (map updateBullet bullets))
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _)
       = magV (rp .- p) < s
      
      ufoCollidesWith :: PointInSpace -> UFO -> Bool
      ufoCollidesWith p (UFO up _ _ _ _)
       = magV (up .- p) < 10      -- up: ufo position, ufo has size 10      

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets
       
      ufoCollidesWithBullet :: UFO -> Bool
      ufoCollidesWithBullet ufo
       = any (\(Bullet bp _ _) -> ufoCollidesWith bp ufo) bullets
      
      collidesWithUFO :: PointInSpace -> UFO -> Bool
      collidesWithUFO p (UFO up _ _ _ _)
       = magV (up .- p) < 10

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
                       
      updateUFO :: UFO -> UFO
      updateUFO (UFO p v h Hunting ft)
        | ufoCollidesWithBullet (UFO p v h state ft)
            = (UFO (shipPos .- (100,100)) v (h-1) Fleeing ft)
        | otherwise 
            = (UFO (shipPos .- (100,100)) v h state ft)
      updateUFO (UFO p v h Fleeing ft)
        | ft > 5
            =(UFO (restoreToScreen (ufoPos .+ timeStep .* (ufoV .+ (rotateV (pi/3) shipPos)))) v h Hunting 0) 
        | otherwise 
            = (UFO (restoreToScreen (ufoPos .+ timeStep .* (ufoV .+ (rotateV (pi/3) shipPos)))) v h state (ft+timeStep))
      updateUFO (UFO p v h Exploding ft)
        | ufoCollidesWithBullet (UFO p v h state ft)
            = (UFO (shipPos .- (100,100)) v (h-1) state ft)
        | otherwise 
            = (UFO (shipPos .- (100,100)) v h state ft)

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)
  
      newUFOPos :: PointInSpace
      newUFOPos = restoreToScreen (ufoPos .+ timeStep .* (ufoV .+ (rotateV (pi/3) shipPos)))
  
splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]
 
destroyUFO :: UFO -> Maybe a
destroyUFO (UFO p v h state ft) = Nothing

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x
    | x < (-400) = 800+x
    | x > 400    = x-800
    | otherwise  = x

drawWorld :: AsteroidWorld -> Picture

drawWorld GameOver 
   = pictures [scale 0.3 0.3 . translate (-400) 0 
               . color red . text $ "Game Over!",
           scale 0.1 0.1 . translate (-1150) (-550)
           . color white . text $ 
           "Click right mousebutton to restart"]
drawWorld Win 
   = pictures [scale 0.3 0.3 . translate (-400) 0 
               . color red . text $ "You won!",
           scale 0.1 0.1 . translate (-1150) (-550)
           . color white . text $ 
           "Click right mousebutton to restart"]

drawWorld (Play rocks (Ship (x,y) (vx,vy)) (UFO (ux,uy) (uvx, uvy) health state ft) bullets)
  = pictures [ship, asteroids, ufo, shots]
   where
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [(color orange (polygon (asteroidShape x y s)))
                         | Rock   (x,y) s _ <- rocks]
    ufo       = color green (pictures [translate ux uy (circle 10)])
    shots     = pictures [translate x y (color red (circle 2))
                         | Bullet (x,y) _ _ <- bullets]
                         
asteroidShape :: Float -> Float -> Float -> [Point]
asteroidShape x y s = [(x,y+s),(x+s,y), (x,y+0.5),(x+0.4*s,y-0.4*s),(x-0.4*s,y+0.5*s),(x-0.2*s,y-0.2*s),(x+0.6*s,y+0.3*s)]

handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

-- new eventhandler for restarting --
handleEvents (EventKey (MouseButton RightButton) Down _ _) GameOver
          = initialWorld
          
handleEvents (EventKey (MouseButton RightButton) Down _ _) Win
          = initialWorld


handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) ufo bullets)
             = Play rocks (Ship shipPos newVel) ufo
                          (newBullet : bullets)
 where
     newBullet = Bullet shipPos
                        (-150 .* norm (shipPos .- clickPos))
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

-- Main function that launches the game --
main = play
         (InWindow "Asteroids!" (550,550) (20,20))
         black
         24
         initialWorld
         drawWorld
         handleEvents
         simulateWorld