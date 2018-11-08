-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world = return world

-- | Handle user input
input :: Event -> World -> IO World
input event World {player = player} =
  case event of
    EventKey (SpecialKey KeyUp   ) Down _ _ -> return (World {player = movePlayerUp player})
    EventKey (SpecialKey KeyDown   ) Down _ _ -> return World {player = player}
    EventKey (SpecialKey KeyLeft   ) Down _ _ -> return World {player = player}
    EventKey (SpecialKey KeyRight   ) Down _ _ -> return World {player = player}
    _ -> return World {player = player}

movePlayerUp :: Player -> Player
movePlayerUp Player {playerSpaceship = spaceship} =
  Player {playerSpaceship = updateSpaceshipPositionInformation spaceship updatedPositionInformation}
  where
    positionInformation = getSpaceshipPosition spaceship
    playerLocation = getLocation positionInformation
    oldX = getXCoordinate playerLocation
    oldY = getYCoordinate playerLocation
    newCoordinates = Coordinate oldX (oldY + 1)
    updatedPositionInformation = updateLocation positionInformation newCoordinates
