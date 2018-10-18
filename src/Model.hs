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
  { speed    :: Int
  , health   :: HealthPoints
  , weapons  :: [Weapon]
  , location :: Coordinates
  }

data Player = Player
  { spaceship :: Spaceship
  , score     :: ScorePoints
  , combo     :: Int
  , comboTime :: Seconds
  }

data Enemy = Enemy
  { bounty          :: ScorePoints
  , enemyCollisionDamage :: DamagePoints
  , evilSpaceship   :: Spaceship
  }

data Obstacle = Obstacle
  { bonusPoints     :: ScorePoints
  , obstacleCollisionDamage :: DamagePoints
  , obstacleHealth      :: HealthPoints
  }

data Item
  = WeaponItem Weapon
  | PowerUp { bonusHealth :: HealthPoints }

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
  }

data Placeable
  = PlaceableCreature Spaceship
  | PlaceableItem Item
  | PlaceableSpaceObstacles Obstacle

data Coordinates = Coordinates
  { x :: Int
  , y :: Int
  }

newtype Spawn =
  Spawn (Placeable, Coordinates)

newtype Level =
  Level [Spawn]

