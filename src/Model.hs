{-# LANGUAGE DeriveGeneric #-}

module Model where

import           GHC.Generics
import           Graphics.Gloss

-- the time a level may take
levelTime :: Int
levelTime = 60

data Spaceship = Spaceship
  { speed                        :: Int
  , health                       :: HealthPoints
  , weapons                      :: [Weapon]
  , spaceshipPositionInformation :: PositionInformation
  , lastHitAtIteration           :: Int
  }

data Player = Player
  { playerSpaceship :: Spaceship
  , score           :: ScorePoints
  , username        :: String
  }

data Enemy = Enemy
  { bounty                 :: ScorePoints
  , enemySpaceship         :: Spaceship
  , aims                   :: Bool
  , shootEveryNthIteration :: Int
  }

data Item
  = WeaponItem { weaponItem         :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }

data Weapon = Weapon
  { bullet              :: Bullet
  , active              :: Bool
  , lastShotAtIteration :: Int
  , reloadTime          :: Int
  }

data Bullet
  = StraightBullet { damage                    :: DamagePoints
                   , hit                       :: Bool
                   , bulletPositionInformation :: PositionInformation
                   , fromPlayer                :: Bool }
  | AimedBullet { damage                    :: DamagePoints
                , hit                       :: Bool
                , bulletPositionInformation :: PositionInformation
                , vector                    :: (Float, Float)
                , fromPlayer                :: Bool
                , step                      :: Float }

data PositionInformation = PositionInformation
  { location    :: Coordinate
  , destination :: Coordinate
  }

data State
  = Menu
  | AskForUsername
  | Playing
  | GameOver
  | GameWin
  | GameDone
  | Quitting
  deriving (Eq)

type DamagePoints = Float

type HealthPoints = Float

type ScorePoints = Float

type Seconds = Float

data World = World
  { player    :: Player
  , enemies   :: [Enemy]
  , bullets   :: [Bullet]
  , level     :: Level
  , camera    :: Camera
  , keyboard  :: Keyboard
  , iteration :: Int
  , state     :: State
  , scores    :: IO [Score]
  }

data Keyboard = Keyboard
  { upKey    :: Bool
  , downKey  :: Bool
  , leftKey  :: Bool
  , rightKey :: Bool
  , shootKey :: Bool
  , pauseKey :: Bool
  , enterKey :: Bool
  } deriving (Show)

data Camera = Camera
  { upperLeftCorner  :: Coordinate
  , upperRightCorner :: Coordinate
  , lowerLeftCorner  :: Coordinate
  , lowerRightCorner :: Coordinate
  }

newtype Level =
  Level [Spawn]

-- Contains the What, Where and When
data Spawn =
  Spawn Placeable
        Time

type Time = Int

data Placeable
  = PlaceableEnemy Enemy
  | PlaceableItem Item

data Coordinate = Coordinate
  { x :: Float
  , y :: Float
  } deriving (Show)

class Locatable a where
  nextLocation :: a -> a

data Images = Images
  { playerImage   :: Picture
  , enemyImage    :: Picture
  }

newtype Scores = Scores
  { unScore :: [Score]
  } deriving (Show)

-- [Score {playerName = "David", playerScore = 1100},Score {playerName = "Paul", playerScore = 1042},Score {playerName = "Dank Pronk", playerScore = 999},Score {playerName = "player4", playerScore = 0},Score {playerName = "player5", playerScore = 0},Score {playerName = "player6", playerScore = 0},Score {playerName = "player7", playerScore = 0},Score {playerName = "player8", playerScore = 0},Score {playerName = "player9", playerScore = 0},Score {playerName = "player10", playerScore = 0}]
data Score = Score
  { playerName  :: String
  , playerScore :: ScorePoints
  } deriving (Generic, Show)
