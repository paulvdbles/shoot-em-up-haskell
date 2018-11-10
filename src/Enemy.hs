module Enemy where

import Model
import Linear.Vector

shootBulletToPlayer :: Enemy -> Player -> Bullet
shootBulletToPlayer enemy player
  | aims enemy = shootAimedBulletToPlayer enemy player
  | otherwise = shootStraightBulletToPlayer enemy player

shootStraightBulletToPlayer :: Enemy -> Player -> Bullet
shootStraightBulletToPlayer enemy player = StraightBullet 10 False (PositionInformation (Coordinate 0 0) (Coordinate 0 0)) False
  where enemyLocationX = x (location (spaceshipPositionInformation (enemySpaceship enemy)))
        enemyLocationY = y (location (spaceshipPositionInformation (enemySpaceship enemy)))
shootAimedBulletToPlayer :: Enemy -> Player -> Bullet
shootAimedBulletToPlayer enemy player = undefined


bulletVector :: Coordinate -> Coordinate -> Coordinate
bulletVector source destination = Coordinate (x destination - x source) (y destination - y source)