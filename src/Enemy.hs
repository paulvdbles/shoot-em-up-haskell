module Enemy where

import           Model

updateShootingEnemies :: World -> World
updateShootingEnemies world = world {bullets = updatedBullets, enemies = updatedEnemies}
  where
    shootingEnemies =
      map (updateShootIterationEnemy world) (filter (determineIfEnemyShouldShoot (iteration world)) (enemies world))
    notShootingEnemies = filter (not . determineIfEnemyShouldShoot (iteration world)) (enemies world)
    updatedBullets = bullets world ++ map (shootBulletFromEnemy (player world)) shootingEnemies
    updatedEnemies = shootingEnemies ++ notShootingEnemies

updateShootIterationEnemy :: World -> Enemy -> Enemy
updateShootIterationEnemy world enemy =
  enemy {enemySpaceship = (enemySpaceship enemy) {weapons = activeWeapon : otherWeapons}}
  where
    activeWeapon = (head (filter active (weapons (enemySpaceship enemy)))) {lastShotAtIteration = iteration world}
    otherWeapons = filter (not . active) (weapons (enemySpaceship enemy))

determineIfEnemyShouldShoot :: Int -> Enemy -> Bool
determineIfEnemyShouldShoot iteration enemy
  | (iteration - lastShot) >= shootEveryNthIteration enemy = True
  | otherwise = False
  where
    lastShot = lastShotAtIteration (head (filter active (weapons (enemySpaceship enemy))))

shootBulletFromEnemy :: Player -> Enemy -> Bullet
shootBulletFromEnemy player enemy
  | aims enemy = shootAimedBulletToPlayer enemy player
  | otherwise = shootStraightBulletToPlayer enemy

shootAimedBulletToPlayer :: Enemy -> Player -> Bullet
shootAimedBulletToPlayer enemy player =
  StraightBullet
    10
    False
    (PositionInformation (Coordinate (x enemyLocation) (y enemyLocation - 55)) vector)
    False
  where
    vector = bulletVector enemyLocation playerLocation
    enemyLocation = location (spaceshipPositionInformation (enemySpaceship enemy))
    playerLocation = location (spaceshipPositionInformation (playerSpaceship player))

bulletVector :: Coordinate -> Coordinate -> Coordinate
bulletVector source destination = Coordinate (x destination - x source) (y destination - y source)

shootStraightBulletToPlayer :: Enemy -> Bullet
shootStraightBulletToPlayer enemy =
  StraightBullet 10 False (PositionInformation (Coordinate enemyLocationX (enemyLocationY - 55)) (Coordinate 0 0)) False
  where
    enemyLocationX = x (location (spaceshipPositionInformation (enemySpaceship enemy)))
    enemyLocationY = y (location (spaceshipPositionInformation (enemySpaceship enemy)))

updatePlayerForAllEnemyBullets :: World -> World
updatePlayerForAllEnemyBullets world = world {bullets = updatedBullets, player = updatedPlayer}
  where
    updatedPlayer = updateHitPlayer (bullets world) world (player world)
    updatedBullets = map (updatePlayerHittingBullet (player world)) (bullets world)

updatePlayerHittingBullet :: Player -> Bullet -> Bullet
updatePlayerHittingBullet player bullet
  | checkIfBulletHitsPlayer player bullet = bullet {hit = True}
  | otherwise = bullet

updateHitPlayer :: [Bullet] -> World -> Player -> Player
updateHitPlayer bullets world player
  | foldr (\b acc -> checkIfBulletHitsPlayer player b || acc) False bullets =
    player
      { playerSpaceship =
          (playerSpaceship player) {lastHitAtIteration = iteration world, health = health (playerSpaceship player) - 10}
      }
  | otherwise = player

checkIfBulletHitsPlayer :: Player -> Bullet -> Bool
checkIfBulletHitsPlayer player bullet =
  (bulletXCoordinate >= enemyLeftBound && bulletXCoordinate <= enemyRightBound) &&
  (bulletYCoordinate >= enemyLowerBound && bulletYCoordinate <= enemyUpperBound) && bulletIsFromEnemy
  where
    bulletXCoordinate = x (location (bulletPositionInformation bullet))
    bulletYCoordinate = y (location (bulletPositionInformation bullet))
    enemyLeftBound = x (location (spaceshipPositionInformation (playerSpaceship player))) - 25
    enemyRightBound = x (location (spaceshipPositionInformation (playerSpaceship player))) + 25
    enemyUpperBound = y (location (spaceshipPositionInformation (playerSpaceship player))) + 40
    enemyLowerBound = y (location (spaceshipPositionInformation (playerSpaceship player))) - 40
    bulletIsFromEnemy = not (fromPlayer bullet)
