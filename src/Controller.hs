-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> World
step secs world = world

-- | Handle user input
input :: Event -> World -> World
input e world = world
