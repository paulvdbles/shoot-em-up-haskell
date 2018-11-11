module Level.Levels where

import           Model

initialPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

seconds :: Time -> Time
seconds n = n * 60

{- SPACESHIPS -}
difficultyLevel :: Int -> HealthPoints
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
    (Spaceship speedSlow (difficultyLevel 1) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)))


mediumDifficultySpaceShip :: Float -> Float -> Placeable
mediumDifficultySpaceShip x y =
  PlaceableSpaceship
    (Spaceship speedSlow (difficultyLevel 2) [weaponNormal] (PositionInformation (Coordinate x y) (Coordinate 0 0)))

{- OBSTACLES -}
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

{- POWERUPS -}
puBonusHealthNormal :: HealthPoints
puBonusHealthNormal = 20

puBonusHealthBoost :: HealthPoints
puBonusHealthBoost = 60

defaultPowerUp :: Float -> Float -> Placeable
defaultPowerUp x y = PlaceableItem (PowerUp puBonusHealthNormal (Coordinate x y))

{- WEAPONS -}
-- also used in SpaceShips
weaponDamageNormal :: Int
weaponDamageNormal = 10

weaponDamageStrong :: Int
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
weaponNormal = Weapon (StraightBullet weaponDamageNormal hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeNormal

weaponStrongSlow :: Weapon
weaponStrongSlow = Weapon (StraightBullet weaponDamageStrong hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeSlow

weaponStrongFast :: Weapon
weaponStrongFast = Weapon (StraightBullet weaponDamageStrong hasBulletHit initialPosition True) isWeaponActive 0 reloadTimeFast

strongWeapon :: Float -> Float -> Placeable
strongWeapon x y = PlaceableItem (WeaponItem weaponStrongSlow (Coordinate x y))

level =
  Level
    [ Spawn (defaultSpaceShip 0 300) (seconds 1)
    , Spawn (defaultSpaceShip 100 400) (seconds 2)
    , Spawn (defaultSpaceShip (-100) 200) (seconds 3)
    , Spawn (defaultSpaceShip 50 300) (seconds 4)
    , Spawn (mediumDifficultySpaceShip 300 300) (seconds 4)
    , Spawn (strongWeapon 0 (-200)) (seconds 5)
    ]
