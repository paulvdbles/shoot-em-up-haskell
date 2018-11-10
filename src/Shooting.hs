module Shooting where

import Model

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

checkIfBulletHitsEnemy :: World -> World
checkIfBulletHitsEnemy world = undefined