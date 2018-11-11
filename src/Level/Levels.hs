module Level.Levels where

import           Level.Spaceships
import           Level.Weapons
import           Model

seconds :: Time -> Time
seconds n = n * 60

nextSecond n = seconds $ n + 1

-- | Only have an n positions to spawn
spawnX n
  | n < 0 = (-340) + 0 * 45
  | n > 15 = (-340) + 15 * 45
  | otherwise = (-340) + n * 45

-- | Just outsize of the screen
spawnY = 500

startingLevel =
  Level
    [ Spawn (defaultEnemy (spawnX 0) spawnY) (seconds 2)
    , Spawn (defaultEnemy (spawnX 1) spawnY) (seconds 3)
    , Spawn (defaultEnemy (spawnX 2) spawnY) (seconds 4)
    , Spawn (defaultEnemy (spawnX 3) spawnY) (seconds 5)
    , Spawn (defaultEnemy (spawnX 4) spawnY) (seconds 6)
    , Spawn (defaultEnemy (spawnX 5) spawnY) (seconds 7)
    , Spawn (defaultEnemy (spawnX 6) spawnY) (seconds 8)
    , Spawn (defaultEnemy (spawnX 7) spawnY) (seconds 9)
    , Spawn (defaultEnemy (spawnX 8) spawnY) (seconds 10)
    , Spawn (defaultEnemy (spawnX 9) spawnY) (seconds 11)
    , Spawn (defaultEnemy (spawnX 10) spawnY) (seconds 12)
    , Spawn (defaultEnemy (spawnX 11) spawnY) (seconds 13)
    , Spawn (defaultEnemy (spawnX 12) spawnY) (seconds 14)
    , Spawn (defaultEnemy (spawnX 13) spawnY) (seconds 15)
    , Spawn (defaultEnemy (spawnX 14) spawnY) (seconds 16)
    , Spawn (defaultEnemy (spawnX 15) spawnY) (seconds 17)
    , Spawn (defaultEnemy (spawnX 15) spawnY) (seconds 18)
    , Spawn (defaultEnemy (spawnX 15) spawnY) (seconds 19)
    , Spawn (defaultEnemy (spawnX 15) spawnY) (seconds 20)
    , Spawn (mediumDifficultyEnemy (spawnX 8) spawnY) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 9) spawnY) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 10) spawnY) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 11) spawnY) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 3) spawnY) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 4) spawnY) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 5) spawnY) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 6) spawnY) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 12) spawnY) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 13) spawnY) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 14) spawnY) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 15) spawnY) (seconds 35)
    , Spawn (defaultEnemy (spawnX 0) spawnY) (seconds 40)
    , Spawn (defaultEnemy (spawnX 1) spawnY) (seconds 42)
    , Spawn (defaultEnemy (spawnX 2) spawnY) (seconds 44)
    , Spawn (defaultEnemy (spawnX 3) spawnY) (seconds 46)
    , Spawn (defaultEnemy (spawnX 4) spawnY) (seconds 48)
    , Spawn (defaultEnemy (spawnX 5) spawnY) (seconds 50)
    , Spawn (defaultEnemy (spawnX 6) spawnY) (seconds 51)
    , Spawn (defaultEnemy (spawnX 7) spawnY) (seconds 52)
    , Spawn (defaultEnemy (spawnX 8) spawnY) (seconds 53)
    , Spawn (mediumDifficultyEnemy (spawnX 5) spawnY) (seconds 55)
    , Spawn (mediumDifficultyEnemy (spawnX 6) spawnY) (seconds 55)
    , Spawn (mediumDifficultyEnemy (spawnX 5) spawnY) (seconds 57)
    , Spawn (mediumDifficultyEnemy (spawnX 6) spawnY) (seconds 57)
    ]
