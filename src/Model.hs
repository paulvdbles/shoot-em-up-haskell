{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import           GHC.Generics
import           Graphics.Gloss

mockPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

pistol = Pistol (Bullet (DamagePoints 10) mockPosition) True 0

initialSpaceship = Spaceship 1 (HealthPoints 100) [pistol] mockPosition

initialPlayer = Player initialSpaceship (ScorePoints 0) 0 (Seconds 0)

emptyLevel = Level []

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialKeyboard :: Keyboard
initialKeyboard = Keyboard False False False False False

initialDisplay :: Display
initialDisplay = InWindow "shoot-em-up-haskell" (720, 960) (0, 0)

initialState :: World
initialState = World initialPlayer [] [] [] emptyLevel mockCamera initialKeyboard 0

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
  = WeaponItem { weaponItem         :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }

data Weapon
  = Pistol { bullet          :: Bullet
           , active          :: Bool
           , lastShotAtFrame :: Int }
  | Laser { bullet          :: Bullet
          , active          :: Bool
          , lastShotAtFrame :: Int }
  | Bazooka { bullet          :: Bullet
            , reloadTime      :: Int
            , active          :: Bool
            , lastShotAtFrame :: Int }

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
  , bullets   :: [Bullet]
  , level     :: Level
  , camera    :: Camera
  , keyboard  :: Keyboard
  , iteration :: Int
  }

data Keyboard = Keyboard
  { upKey    :: Bool
  , downKey  :: Bool
  , leftKey  :: Bool
  , rightKey :: Bool
  , shootKey :: Bool
  } deriving (Show)

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

newtype Scores = Scores
  { unScore :: [Score]
  } deriving (Show)

{- JSON STUFF -}
data Score = Score
  { playername  :: String
  , playerscore :: Int
  } deriving (Generic, Show)

{- END JSON STUFF -}
updateLocation :: PositionInformation -> Coordinate -> PositionInformation
updateLocation PositionInformation {destination = destination} coordinate =
  PositionInformation {location = coordinate, destination = destination}

updateSpaceshipPositionInformation :: Spaceship -> PositionInformation -> Spaceship
updateSpaceshipPositionInformation spaceship position = spaceship {spaceshipPositionInformation = position}

updatePlayersSpaceship :: Player -> Spaceship -> Player
updatePlayersSpaceship player spaceship = player {playerSpaceship = spaceship}

updateWorldsPlayer :: World -> Player -> World
updateWorldsPlayer world player = world {player = player}
