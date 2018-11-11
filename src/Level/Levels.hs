module Level.Levels where

import           Level.Obstacles
import           Level.PowerUps
import           Level.Spaceships
import           Level.Weapons
import           Model

seconds :: Time -> Time
seconds n = n * 60

nextSecond n = seconds $ n + 1

spawnX n
  | n < 0 = (-340) + 0 * 45
  | n > 15 = (-340) + 15 * 45
  | otherwise = (-340) + n * 45

-- Just outsize of the screen
spawnY y = 500

--spawnY y = 300
startingLevel =
  Level
    [ Spawn (defaultEnemy (spawnX 0) (spawnY y)) (seconds 2)
    , Spawn (defaultEnemy (spawnX 1) (spawnY y)) (seconds 3)
    , Spawn (defaultEnemy (spawnX 2) (spawnY y)) (seconds 4)
    , Spawn (defaultEnemy (spawnX 3) (spawnY y)) (seconds 5)
    , Spawn (defaultEnemy (spawnX 4) (spawnY y)) (seconds 6)
    , Spawn (defaultEnemy (spawnX 5) (spawnY y)) (seconds 7)
    , Spawn (defaultEnemy (spawnX 6) (spawnY y)) (seconds 8)
    , Spawn (defaultEnemy (spawnX 7) (spawnY y)) (seconds 9)
    , Spawn (defaultEnemy (spawnX 8) (spawnY y)) (seconds 10)
    , Spawn (defaultEnemy (spawnX 9) (spawnY y)) (seconds 11)
    , Spawn (defaultEnemy (spawnX 10) (spawnY y)) (seconds 12)
    , Spawn (defaultEnemy (spawnX 11) (spawnY y)) (seconds 13)
    , Spawn (defaultEnemy (spawnX 12) (spawnY y)) (seconds 14)
    , Spawn (defaultEnemy (spawnX 13) (spawnY y)) (seconds 15)
    , Spawn (defaultEnemy (spawnX 14) (spawnY y)) (seconds 16)
    , Spawn (defaultEnemy (spawnX 15) (spawnY y)) (seconds 17)
    , Spawn (defaultEnemy (spawnX 15) (spawnY y)) (seconds 18)
    , Spawn (defaultEnemy (spawnX 15) (spawnY y)) (seconds 19)
    , Spawn (defaultEnemy (spawnX 15) (spawnY y)) (seconds 20)
    , Spawn (mediumDifficultyEnemy (spawnX 8) (spawnY y)) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 9) (spawnY y)) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 10) (spawnY y)) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 11) (spawnY y)) (seconds 25)
    , Spawn (mediumDifficultyEnemy (spawnX 3) (spawnY y)) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 4) (spawnY y)) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 5) (spawnY y)) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 6) (spawnY y)) (seconds 30)
    , Spawn (mediumDifficultyEnemy (spawnX 12) (spawnY y)) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 13) (spawnY y)) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 14) (spawnY y)) (seconds 35)
    , Spawn (mediumDifficultyEnemy (spawnX 15) (spawnY y)) (seconds 35)
    , Spawn (defaultEnemy (spawnX 0) (spawnY y)) (seconds 40)
    , Spawn (defaultEnemy (spawnX 1) (spawnY y)) (seconds 42)
    , Spawn (defaultEnemy (spawnX 2) (spawnY y)) (seconds 44)
    , Spawn (defaultEnemy (spawnX 3) (spawnY y)) (seconds 46)
    , Spawn (defaultEnemy (spawnX 4) (spawnY y)) (seconds 48)
    , Spawn (defaultEnemy (spawnX 5) (spawnY y)) (seconds 50)
    , Spawn (defaultEnemy (spawnX 6) (spawnY y)) (seconds 51)
    , Spawn (defaultEnemy (spawnX 7) (spawnY y)) (seconds 52)
    , Spawn (defaultEnemy (spawnX 8) (spawnY y)) (seconds 53)
    , Spawn (mediumDifficultyEnemy (spawnX 5) (spawnY y)) (seconds 55)
    , Spawn (mediumDifficultyEnemy (spawnX 6) (spawnY y)) (seconds 55)
    , Spawn (mediumDifficultyEnemy (spawnX 5) (spawnY y)) (seconds 57)
    , Spawn (mediumDifficultyEnemy (spawnX 6) (spawnY y)) (seconds 57)
    ]
