{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import           GHC.Generics
import           Graphics.Gloss

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
  , lastHitAtIteration       :: Int
  }

data Obstacle = Obstacle
  { bonusPoints                 :: ScorePoints
  , obstacleCollisionDamage     :: DamagePoints
  , obstacleHealth              :: HealthPoints
  , obstaclePositionInformation :: PositionInformation
  }

data Item
  = WeaponItem { weaponItem         :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }

data Weapon
 = Weapon { bullet          :: Bullet
           , active          :: Bool
           , lastShotAtFrame :: Int
           , reloadTime      :: Int }


data Bullet = Bullet
  { damage                    :: DamagePoints
  , hit                       :: Bool
  , bulletPositionInformation :: PositionInformation
  }

data PositionInformation = PositionInformation
  { location    :: Coordinate
  , destination :: Coordinate
  }

data State = Menu | Playing | GameOver | GameWin
  deriving (Eq)

type DamagePoints = Int

type HealthPoints = Int

type ScorePoints = Int

type Seconds = Int

data World = World
  { player    :: Player
  , enemies   :: [Enemy]
  , obstacles :: [Obstacle]
  , bullets   :: [Bullet]
  , level     :: Level
  , camera    :: Camera
  , keyboard  :: Keyboard
  , iteration :: Int
  , state     :: State
  , scores    :: IO (Either String [Score])
  }

data Keyboard = Keyboard
  { upKey    :: Bool
  , downKey  :: Bool
  , leftKey  :: Bool
  , rightKey :: Bool
  , shootKey :: Bool
  , pauseKey :: Bool
  , enterKey :: Bool
  }  deriving (Show)

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

-- spawn moet bevatten: enemy, frame waarop enemy moet spawnen, spawn coordinaten

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

newtype Scores = Scores
  { unScore :: [Score]
  } deriving (Show)

data Score = Score
  { playerName  :: String
  , playerScore :: Int
  } deriving (Generic, Show)