{-# LANGUAGE DeriveAnyClass #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

-- example stuff above

mockPosition = PositionInformation (Coordinate 0 0) (Coordinate 0 0)

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
  } deriving (Renderable)

data Player = Player
  { playerSpaceship :: Spaceship
  , score           :: ScorePoints
  , comboMultiplier :: Int
  , comboTime       :: Seconds
  } deriving (Renderable)

data Enemy = Enemy
  { bounty               :: ScorePoints
  , enemyCollisionDamage :: DamagePoints
  , enemySpaceship       :: Spaceship
  } deriving (Renderable)

data Obstacle = Obstacle
  { bonusPoints                 :: ScorePoints
  , obstacleCollisionDamage     :: DamagePoints
  , obstacleHealth              :: HealthPoints
  , obstaclePositionInformation :: PositionInformation
  } deriving (Renderable)

data Item
  = WeaponItem { weapon             :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }
  deriving (Renderable)

data Weapon
  = Pistol { bullet :: Bullet }
  | Laser { bullet :: Bullet }
  | Bazooka { bullet     :: Bullet
            , reloadTime :: Int }

data Bullet = Bullet
  { damage                    :: DamagePoints
  , bulletPositionInformation :: PositionInformation
  } deriving (Renderable)

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
  { x :: Int
  , y :: Int
  }

class Renderable a where
  render :: a -> a

class Locatable a where
  nextLocation :: a -> a