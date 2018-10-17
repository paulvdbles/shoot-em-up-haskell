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

data Creature = Creature
  { speed   :: Int
  , health  :: HealthPoints
  , weapons :: [Weapon]
  }

data Player = Player
  { score    :: Int
  , creature :: Creature
  }

newtype Enemy =
  Enemy Creature

data Item
  = Weapon
  | PowerUp { bonusHealth :: HealthPoints }

data Weapon
  = Pistol { damage :: Damage }
  | Laser { damage :: Damage }
  | Bazooka { damage :: Damage }

newtype Damage =
  Damage Int

newtype HealthPoints =
  HealthPoints Int

data Placeable = PlaceableCreature Creature | PlaceableItem Item

data Coordinates = Coordinates
  { x    :: Int
  , y    :: Int
  }

newtype Spawn = Spawn (Placeable, Coordinates)

newtype Level = Level [Spawn]

