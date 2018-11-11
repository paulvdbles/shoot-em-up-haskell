module Level.Obstacles where

import           Model

bonusPointsNormal :: ScorePoints
bonusPointsNormal = 5

bonusPointsMore :: ScorePoints
bonusPointsMore = 10

collisionDamageNormal :: DamagePoints
collisionDamageNormal = 10

obstacleHealthNormal :: HealthPoints
obstacleHealthNormal = 20

obstacleHealthHard :: HealthPoints
obstacleHealthHard = 40

-- defaultObstaclePositionInformation
defObsPosIn :: Float -> Float -> PositionInformation
defObsPosIn x y = PositionInformation (Coordinate x y) (Coordinate 0 0)

defaultObstacle :: Float -> Float -> Placeable
defaultObstacle x y =
  PlaceableObstacle (Obstacle bonusPointsNormal collisionDamageNormal obstacleHealthNormal (defObsPosIn x y))
