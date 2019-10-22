-- | Improved Asteroids Game
-- | Authors: Niko Heikkilä, Katri Passi, AP Väisänen  ??
--bulletille tarkistus törmääkö ufon kanssa


module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

-- Define base game area --
data AsteroidWorld = Play [Rock] Ship UFO [Bullet]
                   | GameOver
                   | Suicide        -- New state Suicide if you happen to shoot yourself
                   | Win
                   deriving (Eq,Show)

type Velocity     = (Float, Float)  -- Velocity is a pair with two floats
type Size         = Float           -- Size is Float
type Age          = Float           -- Age is Float
type Health       = Int             -- Health is Int

-- New class Spatial and instances for Ship, Bullet, Rock and UFO --
class Spatial a where
    coord :: a -> PointInSpace
instance Spatial Ship where
    coord (Ship p v) = p
instance Spatial Bullet where
    coord (Bullet p v a) = p
instance Spatial Rock where
    coord (Rock p s v) = p
instance Spatial UFO where
    coord ufo = case ufo of
        (HuntingUFO p v h) -> p
        (FleeingUFO p v h a) -> p
        (ExplodingUFO p v a) -> p

-- Define Ship, Bullet and Rock --
data Ship   = Ship   PointInSpace Velocity
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity
    deriving (Eq,Show)
-- Our UFO --
data UFO = HuntingUFO PointInSpace Velocity Health
         |FleeingUFO PointInSpace Velocity Health Age
         |ExplodingUFO PointInSpace Velocity Age
         deriving (Eq, Show)

ufoHealth ufo = case ufo of
                HuntingUFO _ _ ufoHP -> ufoHP
                FleeingUFO _ _ ufoHP _ -> ufoHP
                ExplodingUFO eUfoPos _ _ -> 0
                


initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)
                   ,Rock (-45,201)  45 (13,-8)
                   ,Rock (45,22)    25 (-2,8)
                   ,Rock (-210,-15) 30 (-2,-8)
                   ,Rock (-45,-201) 25 (8,2)
                   ] -- The default rocks
                   (Ship (0,0) (0,0)) -- The initial ship
                   (HuntingUFO  (75, 75) (2, 5) 3) -- The initial UFO (health=3)
                   [] -- The initial bullets (none)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver
simulateWorld _        Win               = Win
simulateWorld _        Suicide           = Suicide

simulateWorld timeStep (Play rocks (Ship shipPos shipV) ufo bullets)
  | any (checkCollision (Ship shipPos shipV)) rocks = GameOver
  | checkCollision ufo (Ship shipPos shipV) = GameOver
  | any (checkShipBulletCollision (Ship shipPos shipV)) bullets = Suicide
  | (ufoHealth ufo) == 0 = Win
  | otherwise = Play (concatMap updateRock rocks)
                              (Ship newShipPos shipV)
                              (updateUFO ufo)
                              (concat (map updateBullet bullets))
  where
      
      -- New function for checking if two objects collide --
      checkCollision :: (Spatial a, Spatial b) => a -> b -> Bool
      checkCollision x y 
            = if magV (coord x .- coord y) < 10 then True else False
            
      -- Special function needed for ship-bullet collisions because otherwise
      -- you would be shot every time you shoot because the bullets are fired 
      -- from your own position.
      checkShipBulletCollision :: Ship -> Bullet -> Bool
      checkShipBulletCollision ship (Bullet p v a) 
            = if a > 0.1 then checkCollision ship (Bullet p v a) else False
      
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v)
       | any (checkCollision r) bullets && s < 7
            = []
       | any (checkCollision r) bullets && s > 7
            = splitRock r
       | otherwise
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]

      updateBullet :: Bullet -> [Bullet]
      updateBullet (Bullet p v a)
        | a > 5
             = []
        | any (checkCollision (Bullet p v a)) rocks || (checkCollision (Bullet p v a) ufo)
             = []
        | otherwise
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v
                       (a + timeStep)]
                       
      updateUFO :: UFO -> UFO
      updateUFO ufo = case ufo of
                        HuntingUFO ufoPos ufoVel ufoHP -> if any (checkCollision ufo) bullets 
                                                            then (FleeingUFO (restoreToScreen (ufoPos .+ timeStep .* ufoVel)) ufoVel (ufoHP - 1) 0)
                                                            else (HuntingUFO (restoreToScreen(ufoPos .+ timeStep .* newUFOVHunting)) newUFOVHunting ufoHP)
                        FleeingUFO ufoPos ufoVel ufoHP ufoAge -> if (ufoAge > 5)
                                                                    then (HuntingUFO (restoreToScreen(ufoPos .+ timeStep .* newUFOVHunting)) newUFOVHunting ufoHP)
                                                                    else (FleeingUFO (restoreToScreen (ufoPos .+ timeStep .* (ufoVel .+ (rotateV (pi/3) shipPos)))) ufoVel ufoHP (ufoAge + timeStep))
                        ExplodingUFO ufoPos ufoVel ufoAge -> (ExplodingUFO (restoreToScreen (ufoPos .+ timeStep .* ufoVel)) ufoVel (ufoAge + timeStep))

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)
  
      newUFOVHunting = 25 .* (norm (shipPos .- (coord ufo)))
  
  
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
drawWorld Suicide 
   = pictures [scale 0.3 0.3 . translate (-400) 0 
               . color red . text $ "Suicide :(",
           scale 0.1 0.1 . translate (-1150) (-550)
           . color white . text $ 
           "Click right mousebutton to restart"]

drawWorld (Play rocks (Ship (x,y) (vx,vy)) ufo bullets)
  = pictures [ship, asteroids, ufoImage, shots]
   where
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [(color orange (polygon (asteroidShape x y s)))
                         | Rock   (x,y) s _ <- rocks]

    ufoImage  = color green (pictures [translate ux uy (circle 10)])
    ux = (xOfPointInSpace (coord ufo))
    uy = (yOfPointInSpace (coord ufo))
    
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
          
handleEvents (EventKey (MouseButton RightButton) Down _ _) Suicide
          = initialWorld


handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) ufo bullets)
             = Play rocks (Ship shipPos newVel) ufo
                          (newBullet : bullets)
 where
     newBullet = Bullet shipPos
                        ((negate 150) .* norm (shipPos .- clickPos))
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

handleEvents _ w = w

type PointInSpace = (Float, Float)

(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)

xOfPointInSpace :: PointInSpace -> Float
xOfPointInSpace (x,_) = x
yOfPointInSpace :: PointInSpace -> Float
yOfPointInSpace (_,y) = y



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