module Shooting where

import           Model

checkIfPlayerShouldShoot :: World -> World
checkIfPlayerShouldShoot world
  | shootKey (keyboard world) = shootBulletIfWeaponIsReloaded world
  | otherwise = world

shootBulletIfWeaponIsReloaded :: World -> World
shootBulletIfWeaponIsReloaded world
  | iteration' >= lastShotAtFrame weapon + reloadTime weapon = shootBullet world
  | otherwise = world
  where
    iteration' = iteration world
    playerSpaceship' = playerSpaceship (player world)
    weapon = head (filter active (weapons playerSpaceship'))

shootBullet :: World -> World
shootBullet world =
  world
    { player = (player world) {playerSpaceship = (playerSpaceship (player world)) {weapons = updatedWeapons}}
    , bullets = updatedBullets
    }
  where
    playerSpaceship' = playerSpaceship (player world)
    weapon = head (filter active (weapons playerSpaceship'))
    spawnLocation = determineBulletsPositionInformation playerSpaceship'
    updatedBullets = (bullet weapon) {bulletPositionInformation = spawnLocation} : bullets world
    updatedWeapons = weapon {lastShotAtFrame = iteration world} : filter (not . active) (weapons playerSpaceship')

determineBulletsPositionInformation :: Spaceship -> PositionInformation
determineBulletsPositionInformation playerSpaceship = PositionInformation location' destination
  where
    location' = Coordinate (x playerLocation) (y playerLocation + 55)
    destination = Coordinate (x playerLocation) (y playerLocation + 1250) -- Add +1250 so the bullet's destination is outside the screen
    playerLocation = location (spaceshipPositionInformation playerSpaceship)

--updateHitEnemies world = world {enemies = updatedEnemies}
--  where
--    updatedEnemies =
--      [ if checkIfBulletHitsEnemy a b
--        then b {enemySpaceship = (enemySpaceship b) {health = HealthPoints 0}}
--        else b
--      | a <- bullets'
--      , b <- enemies'
--      ]
--    enemies' = enemies world
--    bullets' = bullets world
--
--updateHitBullets world = world {bullets = updatedBullets}
--  where
--    updatedBullets =
--      [ if checkIfBulletHitsEnemy a b
--        then a {hit = True}
--        else a
--      | a <- bullets'
--      , b <- enemies'
--      ]
--    enemies' = enemies world
--    bullets' = bullets world

-- Als een bullet een enemy raakt:
--  - moet de bullet geupdatet worden naar hit = tue
--  - moet de enemy's hp geupdatet worden met damage points van bullet


updateEnemiesForAllBullets :: World -> World
updateEnemiesForAllBullets world = undefined
  where
    results = map (updateEnemyForAllBullets bullets') enemies'
    bullets' = bullets world
    enemies' = enemies world

updateEnemyForAllBullets :: [Bullet] -> Enemy -> [(Enemy, Bullet, Bool)]
updateEnemyForAllBullets bullets enemy = map (checkIfBulletHitsEnemy enemy) bullets

checkIfBulletHitsEnemy :: Enemy -> Bullet -> (Enemy, Bullet, Bool)
checkIfBulletHitsEnemy enemy bullet = (enemy, bullet, hit)
  where
    bulletXCoordinate = x (location (bulletPositionInformation bullet))
    bulletYCoordinate = y (location (bulletPositionInformation bullet))
    enemyLeftBound = x (location (spaceshipPositionInformation (enemySpaceship enemy))) - 20
    enemyRightBound = x (location (spaceshipPositionInformation (enemySpaceship enemy))) + 20
    enemyUpperBound = y (location (spaceshipPositionInformation (enemySpaceship enemy))) + 20
    enemyLowerBound = y (location (spaceshipPositionInformation (enemySpaceship enemy))) - 20
    hit = (bulletXCoordinate >= enemyLeftBound && bulletXCoordinate <= enemyRightBound) &&
            (bulletYCoordinate >= enemyLowerBound && bulletYCoordinate <= enemyUpperBound)
