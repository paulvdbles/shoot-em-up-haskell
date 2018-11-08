{-# LANGUAGE DeriveAnyClass #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy

mockPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

pistol = Pistol (Bullet (DamagePoints 10) mockPosition)

initialSpaceship = Spaceship 1 (HealthPoints 100) [pistol] mockPosition

initialPlayer = Player initialSpaceship (ScorePoints 0) 0 (Seconds 0)

emptyLevel = Level []

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialState :: World
initialState = World initialPlayer [] [] emptyLevel mockCamera

data Spaceship = Spaceship
  { speed                        :: Int
  , health                       :: HealthPoints
  , weapons                      :: [Weapon]
  , spaceshipPositionInformation :: PositionInformation
  }

data Player = Player
  { playerSpaceship :: Spaceship
  , score           :: ScorePoints
  , comboMultiplier :: Int
  , comboTime       :: Seconds
  }

data Enemy = Enemy
  { bounty               :: ScorePoints
  , enemyCollisionDamage :: DamagePoints
  , enemySpaceship       :: Spaceship
  }

data Obstacle = Obstacle
  { bonusPoints                 :: ScorePoints
  , obstacleCollisionDamage     :: DamagePoints
  , obstacleHealth              :: HealthPoints
  , obstaclePositionInformation :: PositionInformation
  }

data Item
  = WeaponItem { weapon             :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }

data Weapon
  = Pistol { bullet :: Bullet }
  | Laser { bullet :: Bullet }
  | Bazooka { bullet     :: Bullet
            , reloadTime :: Int }

data Bullet = Bullet
  { damage                    :: DamagePoints
  , bulletPositionInformation :: PositionInformation
  }

data PositionInformation = PositionInformation
  { location    :: Coordinate
  , destination :: Coordinate
  }

newtype DamagePoints =
  DamagePoints Int
  deriving (Eq)

newtype HealthPoints =
  HealthPoints Int
  deriving (Eq)

newtype ScorePoints =
  ScorePoints Int
  deriving (Eq)

newtype Seconds =
  Seconds Int
  deriving (Eq)

data World = World
  { player    :: Player
  , enemies   :: [Enemy]
  , obstacles :: [Obstacle]
  , level     :: Level
  , camera    :: Camera
  }

data Camera = Camera
  { upperLeftCorner  :: Coordinate
  , upperRightCorner :: Coordinate
  , lowerLeftCorner  :: Coordinate
  , lowerRightCorner :: Coordinate
  }

newtype Level =
  Level [Spawn]

newtype Spawn =
  Spawn (Placeable, Coordinate)

data Placeable
  = PlaceableSpaceship Spaceship
  | PlaceableItem Item
  | PlaceableObstacle Obstacle

data Coordinate = Coordinate
  { x :: Float
  , y :: Float
  }

class Locatable a where
  nextLocation :: a -> a

data Images = Images
  { playerImage   :: Picture
  , enemyImage    :: Picture
  , obstacleImage :: Picture
  }

getSpaceshipPosition :: Spaceship -> PositionInformation
getSpaceshipPosition Spaceship {spaceshipPositionInformation = position} = position

getLocation :: PositionInformation -> Coordinate
getLocation PositionInformation {location = location} = location

getXCoordinate :: Coordinate -> Float
getXCoordinate Coordinate {x = x} = x

getYCoordinate :: Coordinate -> Float
getYCoordinate Coordinate {y = y} = y

updateLocation :: PositionInformation -> Coordinate -> PositionInformation
updateLocation PositionInformation {destination = destination} coordinate =
  PositionInformation {location = coordinate, destination = destination}

updateSpaceshipPositionInformation :: Spaceship -> PositionInformation -> Spaceship
updateSpaceshipPositionInformation spaceship position = Spaceship {spaceshipPositionInformation = position}

updatePlayersSpaceship :: Player -> Spaceship -> Player
updatePlayersSpaceship player spaceship = Player {playerSpaceship = spaceship}

updateWorldsPlayer :: World -> Player -> World
updateWorldsPlayer world player = World {player = player}