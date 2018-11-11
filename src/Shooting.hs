module Shooting where

import           Model

checkIfPlayerShouldShoot :: World -> World
checkIfPlayerShouldShoot world
  | shootKey (keyboard world) = shootBulletIfWeaponIsReloaded world
  | otherwise = world

shootBulletIfWeaponIsReloaded :: World -> World
shootBulletIfWeaponIsReloaded world
  | iteration' >= lastShotAtIteration weapon' + reloadTime weapon' = shootBulletFromPlayer world
  | otherwise = world
  where
    iteration' = iteration world
    playerSpaceship' = playerSpaceship (player world)
    weapon' = weapon playerSpaceship'

shootBulletFromPlayer :: World -> World
shootBulletFromPlayer world =
  world
    { player = (player world) {playerSpaceship = (playerSpaceship (player world)) {weapon = updatedWeapon}}
    , bullets = updatedBullets
    }
  where
    playerSpaceship' = playerSpaceship (player world)
    spawnLocation = determineBulletsPositionInformation playerSpaceship'
    updatedBullets = (bullet (weapon playerSpaceship')) {bulletPositionInformation = spawnLocation} : bullets world
    updatedWeapon = (weapon playerSpaceship') {lastShotAtIteration = iteration world}

determineBulletsPositionInformation :: Spaceship -> PositionInformation
determineBulletsPositionInformation playerSpaceship = PositionInformation location' destination
  where
    location' = Coordinate (x playerLocation) (y playerLocation + 55)
    destination = Coordinate (x playerLocation) (y playerLocation + 1250) -- Add +1250 so the bullet's destination is outside the screen
    playerLocation = location (spaceshipPositionInformation playerSpaceship)

updateEnemiesForAllBullets :: World -> World
updateEnemiesForAllBullets world =
  world
    { bullets = updatedBullets
    , enemies = updatedEnemies
    , player = (player world) {score = score (player world) + updatedScore}
    }
  where
    bullets' = bullets world
    enemies' = enemies world
    updatedBullets = map (updateHitBullet enemies') bullets'
    updatedEnemies = map (updateHitEnemy bullets' world) enemies'
    updatedScore = updatePlayerScore updatedEnemies

updatePlayerScore :: [Enemy] -> ScorePoints
updatePlayerScore enemies = score
  where
    dead = filter (\enemy -> health (enemySpaceship enemy) <= 0) enemies
    score = foldr (\e acc -> bounty e + acc) 0 dead

updateHitEnemy :: [Bullet] -> World -> Enemy -> Enemy
updateHitEnemy bullets world enemy
  | foldr (\b acc -> checkIfBulletHitsEnemy enemy b || acc) False bullets =
    enemy
      { enemySpaceship =
          (enemySpaceship enemy) {lastHitAtIteration = iteration world, health = health (enemySpaceship enemy) - 10}
      }
  | otherwise = enemy

updateHitBullet :: [Enemy] -> Bullet -> Bullet
updateHitBullet enemies bullet
  | foldr (\e acc -> checkIfBulletHitsEnemy e bullet || acc) False enemies = bullet {hit = True}
  | otherwise = bullet

checkIfBulletHitsEnemy :: Enemy -> Bullet -> Bool
checkIfBulletHitsEnemy enemy bullet =
  (bulletXCoordinate >= enemyLeftBound && bulletXCoordinate <= enemyRightBound) &&
  (bulletYCoordinate >= enemyLowerBound && bulletYCoordinate <= enemyUpperBound) && bulletIsFromPlayer
  where
    bulletXCoordinate = x (location (bulletPositionInformation bullet))
    bulletYCoordinate = y (location (bulletPositionInformation bullet))
    enemyLeftBound = x (location (spaceshipPositionInformation (enemySpaceship enemy))) - 20
    enemyRightBound = x (location (spaceshipPositionInformation (enemySpaceship enemy))) + 20
    enemyUpperBound = y (location (spaceshipPositionInformation (enemySpaceship enemy))) + 20
    enemyLowerBound = y (location (spaceshipPositionInformation (enemySpaceship enemy))) - 20
    bulletIsFromPlayer = fromPlayer bullet
