module Movement where

import           Model

checkIfPlayerShouldBeMoved :: World -> World
checkIfPlayerShouldBeMoved world
  | upKey (keyboard world) = world {player = movePlayer (player world) (calculateUpCoordinate (player world))}
  | downKey (keyboard world) = world {player = movePlayer (player world) (calculateDownCoordinate (player world))}
  | leftKey (keyboard world) = world {player = movePlayer (player world) (calculateLeftCoordinate (player world))}
  | rightKey (keyboard world) = world {player = movePlayer (player world) (calculateRightCoordinate (player world))}
  | otherwise = world

calculateUpCoordinate :: Player -> Coordinate
calculateUpCoordinate player = calculateCoordinate player 0 10

calculateDownCoordinate :: Player -> Coordinate
calculateDownCoordinate player = calculateCoordinate player 0 (-10)

calculateLeftCoordinate :: Player -> Coordinate
calculateLeftCoordinate player = calculateCoordinate player (-10) 0

calculateRightCoordinate :: Player -> Coordinate
calculateRightCoordinate player = calculateCoordinate player 10 0

calculateCoordinate :: Player -> Float -> Float -> Coordinate
calculateCoordinate player offsetX offsetY = Coordinate (oldX + offsetX) (oldY + offsetY)
  where
    playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
    oldX = x playerLocation
    oldY = y playerLocation

movePlayer :: Player -> Coordinate -> Player
movePlayer player newCoordinate =
  if not (playerHitLevelBounds newCoordinate)
    then player {playerSpaceship = (playerSpaceship player) {spaceshipPositionInformation = updatedPositionInformation}}
    else player
  where
    updatedPositionInformation = (spaceshipPositionInformation (playerSpaceship player)) {location = newCoordinate}

updateBullets :: World -> World
updateBullets world = world {bullets = map moveBullet (removeOldBullets (bullets world))}

moveEnemies :: World -> World
moveEnemies world = world{enemies = moved}
  where moved = map moveEnemy (enemies world)

moveEnemy :: Enemy -> Enemy
moveEnemy enemy =
  enemy
    { enemySpaceship =
        (enemySpaceship enemy)
          { spaceshipPositionInformation =
              (spaceshipPositionInformation (enemySpaceship enemy)) {location = newLocation}
          }
    }
  where
    currentLocation = location (spaceshipPositionInformation (enemySpaceship enemy))
    newLocation = Coordinate (x currentLocation) (y currentLocation - 1)

removeOldBullets :: [Bullet] -> [Bullet]
removeOldBullets = filter (not . bulletIsOutsideBounds)

bulletIsOutsideBounds :: Bullet -> Bool
bulletIsOutsideBounds bullet = outsideLeftBound || outsideRightBound || outsideLowerBound || outsideUpperBound
  where
    bulletLocation = location (bulletPositionInformation bullet)
    outsideLeftBound = -360 > x bulletLocation
    outsideRightBound = 360 < x bulletLocation
    outsideLowerBound = -480 > y bulletLocation
    outsideUpperBound = 480 < y bulletLocation

moveBullet :: Bullet -> Bullet
moveBullet bullet@StraightBullet {} = moveBulletStraight bullet
moveBullet bullet@AimedBullet {}    = moveBulletAimed bullet

moveBulletStraight :: Bullet -> Bullet
moveBulletStraight bullet =
  bullet {bulletPositionInformation = (bulletPositionInformation bullet) {location = newLocation}}
  where
    currentLocation = location (bulletPositionInformation bullet)
    newLocation = Coordinate (x currentLocation) (y currentLocation + direction)
    direction
      | fromPlayer bullet = 10
      | otherwise = -10

moveBulletAimed :: Bullet -> Bullet
moveBulletAimed bullet =
  bullet {bulletPositionInformation = (bulletPositionInformation bullet) {location = newLocation}}
  where
    (xVector, yVector) = vector bullet
    currentLocation = location (bulletPositionInformation bullet)
    newLocation = Coordinate (x currentLocation + (xVector / step bullet)) (y currentLocation + (yVector / step bullet))

playerHitLevelBounds :: Coordinate -> Bool
playerHitLevelBounds newLocation = leftBoundHit || rightBoundHit || lowerBoundHit || upperBoundHit
  where
    leftBoundHit = (-360 + 25) > x newLocation
    rightBoundHit = (360 - 25) < x newLocation
    lowerBoundHit = (-480 + 40) > y newLocation
    upperBoundHit = (480 - 40) < y newLocation
