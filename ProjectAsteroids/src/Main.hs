module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Picture

-- UFO added here
data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo
                   | GameOver
                   deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float
type Health     = Int


data Ship   = Ship   PointInSpace Velocity
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity
    deriving (Eq,Show)
-- Added UFO with different constructors
--data Ufo  = Ufo  PointInSpace Velocity
--    deriving (Eq,Show)
data Ufo = Hunting PointInSpace Velocity Health
          | Fleeing PointInSpace Velocity Health
          | Exploding PointInSpace | Waiting PointInSpace Health
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
                   (Hunting (20,0) (0,5) 4) -- The initial Ufo (added)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver

-- UFO added here
-- Tehdään UFOlle neljä tilaa. Aluksi se on Hunting-tilassa, jossa
-- se liikkuu alusta kohti.
-- Jos UFOa ammutaan, se siirtyy Fleeing-tilaan, jossa se liikkuu
-- pois päin aluksesta.
-- Aina kun ufoa ammutaan, sen health-arvo, joka on alussa 4, pienenee
-- yhdellä.
-- Jos UFOn health-arvo menee pienemmäksi kuin 1, siirrytään Exploding-tilaan.
-- Jos ufoa ammutaan, kun se on Fleeing-tilassa, se menee Waiting-tilaan eli
-- jähmettyy paikalleen.
-- Jos ufoa ammutaan Waiting-tilassa, se menee takaisin Hunting-tilaan.
-- Exploding-tilassa Ufo räjähtää eli jää paikalleen ja muuttuu oranssiksi,
-- eikä enää pääse muuhun tilaan.
-- TODO
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets ufo)
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks)
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              (updateUfo ufo) -- ufo added
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _)
       = magV (rp .- p) < s

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets

-- added collidesfunction
      collidesWithBulletUfo :: Ufo ->  Bool
      collidesWithBulletUfo ufo
       = any (\(Bullet bp _ _) -> collidesWithUfo bp ufo) bullets

      collidesWithUfo :: PointInSpace -> Ufo -> Bool
      collidesWithUfo pos ufo = case ufo of
         Hunting x _ _ -> True
         Fleeing x _ _ -> True
         Waiting x _ -> True
         Exploding _ -> False

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

-- ADDED updateUfo function that updates in which state the ufo is in
      updateUfo :: Ufo -> Ufo
      updateUfo ufox = case ufox of
         Hunting pos v health
            | collidesWithBulletUfo ufox -> Fleeing (restoreToScreen (pos .+ timeStep .* v))
              (magV(shipV).*norm(pos .- shipPos)) (health-1)
            | health<1 -> Exploding pos
            | otherwise -> Hunting (restoreToScreen (pos .+ timeStep .* v))
              (magV(shipV).*norm(shipPos .- pos)) (health)
         Fleeing pos v health
            | collidesWithBulletUfo ufox -> Waiting (restoreToScreen (pos .+ timeStep .* v))
              (health - 1)
            | health<1 -> Exploding pos
            | otherwise -> Fleeing (restoreToScreen (pos .+ timeStep .* v))
               v health
         Waiting pos health
            | collidesWithBulletUfo ufox -> Hunting (restoreToScreen (pos))
              (magV(shipV).*norm(pos .- shipPos)) (health-1)
            | health<1 -> Exploding pos
            | otherwise -> Waiting (restoreToScreen (pos)) (health)
         Exploding (xu,yu) -> Exploding (xu,yu)

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

-- UFO position removed here

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

-- ADDED
-- Changed bullets to blue rectangles
-- Changed shapes and colors
-- added different UFO types
drawWorld (Play rocks (Ship (x,y) (vx,vy))  bullets ufotype )
  = pictures [ship, asteroids, shots, ufo]
   where
    ship      = color red (pictures [translate x y (circleSolid 10)]) -- changed
    asteroids = pictures [translate x y (color yellow (circle s))
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color blue (rectangleSolid 10 10))
                         | Bullet (x,y) _ _ <- bullets] -- changed
    ufo       = case ufotype of
      Hunting (xu, yu) _ _ -> color green (pictures [translate xu yu (rectangleSolid 20 15)])
      Fleeing (xu, yu) _ _ -> color magenta (pictures [translate xu yu (rectangleSolid 20 15)])
      Waiting (xu, yu) _ -> color cyan (pictures [translate xu yu (rectangleSolid 20 15)])
      Exploding (xu, yu) -> color orange (pictures [translate xu yu (rectangleSolid 20 20)])

handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

-- ADDED
-- Now it is possible to start a new game after "game over" with space
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) GameOver = initialWorld

-- ufo tännekin
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

-- UFO movement added here and removed

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
