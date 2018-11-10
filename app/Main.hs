module Main where

import           Controller
import           FileSystem
import           Model
import           View

import           Graphics.Gloss.Interface.IO.Game

mockPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

pistol = Pistol (Bullet (DamagePoints 10) mockPosition) True 0 10

initialSpaceship = Spaceship 1 (HealthPoints 100) [pistol] mockPosition

initialPlayer = Player initialSpaceship (ScorePoints 0) 0 (Seconds 0)

emptyLevel = Level []

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialKeyboard :: Keyboard
initialKeyboard = Keyboard False False False False False False

initialState :: World
initialState = World initialPlayer [] [] [] emptyLevel mockCamera initialKeyboard 0 Playing readScoreFile

initialDisplay :: Display
initialDisplay = InWindow "shoot-em-up-haskell" (720, 960) (0, 0)

main :: IO ()
main =
  playIO
    initialDisplay
    black -- Background color
    60   -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
