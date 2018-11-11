module Main where

import           Controller
import           FileSystem
import           Graphics.Gloss.Interface.IO.Game
import           Level.Levels
import           Model
import           View
import           Enemy

initialPlayerPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

gun = Weapon (StraightBullet 10 False initialPlayerPosition True) True 0 10

initialSpaceship = Spaceship 1 100 gun initialPlayerPosition 0

initialPlayer = Player initialSpaceship 0 ""

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialKeyboard :: Keyboard
initialKeyboard = Keyboard False False False False False False False

initialState :: World
initialState = World initialPlayer  [] [] Level.Levels.startingLevel mockCamera initialKeyboard 0 AskForUsername readScoreFile

main :: IO ()
main =
  playIO
    (InWindow "shoot-em-up-haskell" (720, 960) (600, 0))
    black           -- Background color
    60              -- Frames per second
    initialState    -- Initial state
    view            -- View function
    input           -- Event function
    Controller.step -- Step function
