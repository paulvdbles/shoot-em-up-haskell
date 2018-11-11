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
    [ Spawn (defaultEnemy 0 300) (seconds 1)
    , Spawn (defaultEnemy 100 400) (seconds 2)
    , Spawn (defaultEnemy (-100) 200) (seconds 3)
    , Spawn (defaultEnemy 50 300) (seconds 4)
    , Spawn (mediumDifficultyEnemy 300 300) (seconds 4)
    , Spawn (strongWeapon 0 (-200)) (seconds 5)
    ]
