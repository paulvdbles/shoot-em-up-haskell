module Level.Spaceships where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Level.Weapons
import           Model
import           System.Random

difficultyLevel :: Float -> HealthPoints
difficultyLevel n = n * 10

speedSlow :: Int
speedSlow = 1

speedNormal :: Int
speedNormal = 2

speedFast :: Int
speedFast = 3

getColor :: IO Color
getColor = do
  n <- randomRIO (0, 13)
  return $ getc n

getStaticColor :: Int -> IO Color
getStaticColor n
  | n == 0 = return azure
  | otherwise = return green

getc :: Int -> Color
getc n
  | n == 0 = black
  | n == 1 = white
  | n == 2 = red
  | n == 4 = blue
  | n == 5 = yellow
  | n == 6 = cyan
  | n == 7 = magenta
  | n == 8 = rose
  | n == 9 = violet
  | n == 11 = aquamarine
  | n == 12 = chartreuse
  | otherwise = orange

defaultEnemy :: Float -> Float -> Placeable
defaultEnemy x y =
  PlaceableEnemy
    (Enemy
       bounty
       (Spaceship speedSlow (difficultyLevel 2) weaponNormal (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0)
       aims
       shootEveryNthIteration
       (getStaticColor 0)
       getColor)
  where
    bounty = 10
    aims = True
    shootEveryNthIteration = 180

mediumDifficultyEnemy :: Float -> Float -> Placeable
mediumDifficultyEnemy x y =
  PlaceableEnemy
    (Enemy
       bounty
       (Spaceship speedSlow (difficultyLevel 3) weaponNormal (PositionInformation (Coordinate x y) (Coordinate 0 0)) 0)
       aims
       shootEveryNthIteration
       (getStaticColor 1)
       getColor)
  where
    bounty = 10
    aims = False
    shootEveryNthIteration = 120
