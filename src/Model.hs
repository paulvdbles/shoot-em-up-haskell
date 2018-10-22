{-# LANGUAGE DeriveAnyClass #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy

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

class Locatable a where
    nextLocation :: a -> a

class Renderable a where
    render :: a -> IO Picture

instance Renderable Player
    where render p = do
                     picture <- loadJuicyPNG "sprites/player.png"
                     case picture of
                      Just picture -> return picture
                      Nothing -> undefined

instance Renderable Enemy
    where render p = do
                     picture <- loadJuicyPNG "sprites/enemy.png"
                     case picture of
                      Just picture -> return picture
                      Nothing -> undefined

instance Renderable Obstacle
    where render p = do
                     picture <- loadJuicyPNG "sprites/obstacle.png"
                     case picture of
                      Just picture -> return picture
                      Nothing -> undefined






