module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo
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
data Ufo    = Ufo    PointInSpace Velocity
    deriving (Eq,Show)

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
                   (Ufo (200,200) (20,-40))


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver  

simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets (Ufo ufoPos ufoV)) 
  -- | any (collidesWith shipPos) rocks || collidesWithUfo shipPos ufoPos = GameOver
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              (Ufo newUfoPos ufoV)
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
       
      -- collidesWithUfo :: PointInSpace -> PointInSpace -> Bool
      -- collidesWithUfo shipPos ufoPos
        -- = magV (shipPos .- ufoPos) < 30
        
      -- ufoShot :: Bullet -> PointInSpace -> Bool
      -- ufoShot (Bullet p v a) ufoPos
        -- = any magV(p .- ufoPos) < 20
     
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
      
      newUfoPos :: PointInSpace
      newUfoPos = restoreToScreen (ufoPos .+ timeStep .*(ufoV + (rotateV (pi/3) shipPos)))
      
      
      
      -- newUfoVel :: PointInSpace
      -- newUfoVel = (rotateV (0.05) ufoV)

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
     $ "Game Over! Click to start over."
     
    
     
drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets (Ufo (xUfo, yUfo) (vxUfo,vyUfo)))
  = pictures [ship, asteroids,shots, ufo]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [translate x y (scale (s/20) (s/20) (color orange rockPicture)) 
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color red (circle 2)) 
                         | Bullet (x,y) _ _ <- bullets]
    ufo       = color green (pictures [translate yUfo yUfo (thickCircle 20 15)])

rockPicture :: Picture
rockPicture = polygon [(0,0),(10,0),(25,25),(0,40),(-25,25),(-10,0),(0,0)]


handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos) (GameOver) = initialWorld

handleEvents _ GameOver = GameOver

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
