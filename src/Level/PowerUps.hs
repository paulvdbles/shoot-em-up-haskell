module Level.PowerUps where

import           Model

puBonusHealthNormal :: HealthPoints
puBonusHealthNormal = 20

puBonusHealthBoost :: HealthPoints
puBonusHealthBoost = 60

defaultPowerUp :: Float -> Float -> Placeable
defaultPowerUp x y = PlaceableItem (PowerUp puBonusHealthNormal (Coordinate x y))
