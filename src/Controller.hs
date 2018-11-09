-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world = do
  print (keyboard world)
  return (checkIfPlayerShouldBeMoved world)

checkIfPlayerShouldBeMoved :: World -> World
checkIfPlayerShouldBeMoved world
  | upKey (keyboard world) = world {player = movePlayerUp (player world)}
--  | downKey (keyboard world) = world {player = movePlayerDown (player world)}
--  | leftKey (keyboard world) = world {player = movePlayerLeft (player world)}
--  | leftKey (keyboard world) = world {player = movePlayerRight (player world)}
  | otherwise = world

--    downKeyPressed -> undefined
--    leftKeyPressed -> undefined
--    rightKeyPressed -> undefined


-- | Handle user input
input :: Event -> World -> IO World
input event world =
  case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> return (world {keyboard = (keyboard world) {upKey = True}})
    EventKey (SpecialKey KeyUp) Up _ _ -> return (world {keyboard = (keyboard world) {upKey = False}})
    EventKey (SpecialKey KeyDown) Down _ _ -> return (world {keyboard = (keyboard world) {downKey = True}})
    EventKey (SpecialKey KeyDown) Up _ _ -> return (world {keyboard = (keyboard world) {downKey = False}})
    EventKey (SpecialKey KeyLeft) Down _ _ -> return (world {keyboard = (keyboard world) {leftKey = True}})
    EventKey (SpecialKey KeyLeft) Up _ _ -> return (world {keyboard = (keyboard world) {leftKey = False}})
    EventKey (SpecialKey KeyRight) Down _ _ -> return (world {keyboard = (keyboard world) {rightKey = True}})
    EventKey (SpecialKey KeyRight) Up _ _ -> return (world {keyboard = (keyboard world) {rightKey = False}})
    _ -> return world

movePlayerUp :: Player -> Player
movePlayerUp player =
  player {playerSpaceship = updateSpaceshipPositionInformation playerSpaceship' updatedPositionInformation}
  where
    playerSpaceship' = playerSpaceship player
    positionInformation = spaceshipPositionInformation playerSpaceship'
    playerLocation = location positionInformation
    oldX = x playerLocation
    oldY = y playerLocation
    newCoordinates = Coordinate oldX (oldY + 5) -- TODO: Speed gebruiken
    updatedPositionInformation = updateLocation positionInformation newCoordinates

