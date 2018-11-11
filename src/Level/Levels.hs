module Level.Levels where

import           Model
import           Level.Obstacles
import           Level.PowerUps
import           Level.Spaceships
import           Level.Weapons

seconds :: Time -> Time
seconds n = n * 60

level =
  Level
    [ Spawn (defaultSpaceShip 0 300) (seconds 1)
    , Spawn (defaultSpaceShip 100 400) (seconds 2)
    , Spawn (defaultSpaceShip (-100) 200) (seconds 3)
    , Spawn (defaultSpaceShip 50 300) (seconds 4)
    , Spawn (mediumDifficultySpaceShip 300 300) (seconds 4)
    , Spawn (strongWeapon 0 (-200)) (seconds 5)
    ]
