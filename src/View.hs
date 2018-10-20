-- | This module defines how to turn
--   the game state into a picture
module View where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           Model

view :: World -> IO Picture
view world = loadSprite

loadSprite :: IO Picture
loadSprite = do
  Just picture <- loadJuicyPNG "sprites/player.png"
  return picture
