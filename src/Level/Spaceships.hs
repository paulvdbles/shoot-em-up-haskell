module Level.Spaceships where

import           Model
import           Level.Weapons

difficultyLevel :: Float -> HealthPoints
difficultyLevel n = n * 10

speedSlow :: Int
speedSlow = 1

speedNormal :: Int
speedNormal = 2

speedFast :: Int
speedFast = 3

defaultEnemy :: Float -> Float -> Placeable
defaultEnemy x y =
  PlaceableEnemy
    (Enemy bounty collisionDamage (Spaceship speedSlow (difficultyLevel 1) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0) aims shootEveryNthIteration)
  where bounty = 10
        collisionDamage = 10
        aims = True
        shootEveryNthIteration = 180

mediumDifficultyEnemy :: Float -> Float -> Placeable
mediumDifficultyEnemy x y =
  PlaceableEnemy
   (Enemy bounty collisionDamage (Spaceship speedSlow (difficultyLevel 2) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0) aims shootEveryNthIteration)
  where bounty = 10
        collisionDamage = 20
        aims = False
        shootEveryNthIteration = 120
