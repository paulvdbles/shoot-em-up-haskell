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
updateBullets world = world {bullets = map moveBulletToDestination (removeOldBullets (bullets world))}

removeOldBullets :: [Bullet] -> [Bullet]
removeOldBullets = filter bulletShouldBeKept

bulletShouldBeKept :: Bullet -> Bool
bulletShouldBeKept bullet =
  y (location (bulletPositionInformation bullet)) <= y (destination (bulletPositionInformation bullet))

moveBulletToDestination :: Bullet -> Bullet
moveBulletToDestination bullet = bullet {bulletPositionInformation = updatedPositionInformation}
  where
    currentLocation = location (bulletPositionInformation bullet)
    newLocation = Coordinate (x currentLocation) (y currentLocation + 10)
    destination' = destination (bulletPositionInformation bullet)
    updatedPositionInformation = PositionInformation newLocation destination'

playerHitLevelBounds :: Coordinate -> Bool
playerHitLevelBounds newLocation = leftBoundHit || rightBoundHit || lowerBoundHit || upperBoundHit
  where
    leftBoundHit = (-360 + 25) > x newLocation
    rightBoundHit = (360 -25) < x newLocation
    lowerBoundHit = (-480 + 40) > y newLocation
    upperBoundHit = (480- 40) < y newLocation
