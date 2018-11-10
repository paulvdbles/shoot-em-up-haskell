module Main where

import           Controller
import           FileSystem
import           Model
import           View

import           Graphics.Gloss.Interface.IO.Game

initialPlayerPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

gun = Weapon (Bullet 10 False initialPlayerPosition) True 0 10

initialSpaceship = Spaceship 1 100 [gun] initialPlayerPosition

initialPlayer = Player initialSpaceship 0 0 0

emptyLevel = Level []

mockCamera = Camera (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0) (Coordinate 0 0)

initialKeyboard :: Keyboard
initialKeyboard = Keyboard False False False False False False False

testEnemyPosition = PositionInformation (Coordinate 0 300) (Coordinate 0 (-200))

enemy :: Enemy
enemy = Enemy 10 10 (Spaceship 1 100 [gun] testEnemyPosition) (-1)

initialState :: World
initialState = World initialPlayer [enemy] [] [] emptyLevel mockCamera initialKeyboard 0 Playing readScoreFile

main :: IO ()
main =
  playIO
    (InWindow "shoot-em-up-haskell" (720, 960) (600, 0))
    black -- Background color
    60   -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function
