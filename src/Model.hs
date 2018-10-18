-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState
  { infoToShow  :: InfoToShow
  , elapsedTime :: Float
  }

initialState :: GameState
initialState = GameState ShowNothing 0

-- example stuff above
data Spaceship = Spaceship
  { speed   :: Int
  , health  :: HealthPoints
  , weapons :: [Weapon]
  }

data Player = Player
  { playerSpaceship :: Spaceship
  , score           :: ScorePoints
  , comboMultiplier :: Int
  , comboTime       :: Seconds
  , location        :: Coordinate
  }

data Enemy = Enemy
  { bounty               :: ScorePoints
  , enemyCollisionDamage :: DamagePoints
  , enemySpaceship       :: Spaceship
  -- in een aparte location data type
  , enemyLocation        :: Coordinate
  , enemyDestination     :: Coordinate
  }

data Obstacle = Obstacle
  { bonusPoints             :: ScorePoints
  , obstacleCollisionDamage :: DamagePoints
  , obstacleHealth          :: HealthPoints
  -- in een aparte location data type
  , obstacleLocation        :: Coordinate
  , obstacleDestination     :: Coordinate
  }

data Item
  = WeaponItem { weapon             :: Weapon
               , weaponItemLocation :: Coordinate }
  | PowerUp { bonusHealth     :: HealthPoints
            , powerUpLocation :: Coordinate }

data Weapon
  = Pistol { damage :: DamagePoints }
  | Laser { damage :: DamagePoints }
  | Bazooka { damage     :: DamagePoints
            , reloadTime :: Int }

newtype DamagePoints =
  DamagePoints Int

newtype HealthPoints =
  HealthPoints Int

newtype ScorePoints =
  ScorePoints Int

newtype Seconds =
  Seconds Int

data World = World
  { player    :: Player
  , enemies   :: [Enemy]
  , obstacles :: [Obstacle]
  , level     :: Level
  -- camera
  }
  -- renderable dingen in hun eigen renderable type class
  -- location dingen in hun eigen location type class

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
