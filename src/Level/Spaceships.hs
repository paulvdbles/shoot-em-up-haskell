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

defaultSpaceShip :: Float -> Float -> Placeable
defaultSpaceShip x y =
  PlaceableSpaceship
    (Spaceship speedSlow (difficultyLevel 1) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0)


mediumDifficultySpaceShip :: Float -> Float -> Placeable
mediumDifficultySpaceShip x y =
  PlaceableSpaceship
    (Spaceship speedSlow (difficultyLevel 2) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0)
