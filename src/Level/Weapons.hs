module Level.Weapons where

import           Model

initialPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

-- also used in SpaceShips
weaponDamageNormal :: Float
weaponDamageNormal = 10

weaponDamageStrong :: Float
weaponDamageStrong = 25

hasBulletHit :: Bool
hasBulletHit = False

isWeaponActive :: Bool
isWeaponActive = True

reloadTimeFast :: Int
reloadTimeFast = 20

reloadTimeNormal :: Int
reloadTimeNormal = 10

reloadTimeSlow :: Int
reloadTimeSlow = 5

weaponNormal :: Weapon
weaponNormal =
  Weapon (StraightBullet weaponDamageNormal hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeNormal

weaponStrongSlow :: Weapon
weaponStrongSlow =
  Weapon (StraightBullet weaponDamageStrong hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeSlow

weaponStrongFast :: Weapon
weaponStrongFast =
  Weapon (StraightBullet weaponDamageStrong hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeFast
