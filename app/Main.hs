module Main where

import           Controller
import           FileSystem
import           Model
import           View

import           Graphics.Gloss.Interface.IO.Game

initialPlayerPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

pistol = Pistol (Bullet (DamagePoints 10) initialPlayerPosition) True 0 10

initialSpaceship = Spaceship 1 (HealthPoints 100) [pistol] initialPlayerPosition

initialPlayer = Player initialSpaceship (ScorePoints 0) 0 (Seconds 0)

emptyLevel = Level []

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialKeyboard :: Keyboard
initialKeyboard = Keyboard False False False False False False False

testEnemyPosition = PositionInformation (Coordinate 0 300) (Coordinate 0 (-200))

enemy :: Enemy
enemy = Enemy (ScorePoints 10) (DamagePoints 10) (Spaceship 1 (HealthPoints 100) [pistol] testEnemyPosition)

initialState :: World
initialState = World initialPlayer [enemy] [] [] emptyLevel mockCamera initialKeyboard 0 Playing readScoreFile

main :: IO ()
main =
  playIO
    (InWindow "shoot-em-up-haskell" (720, 960) (0, 0))
    black -- Background color
    60   -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
