-- | This module defines how to turn
--   the game state into a picture
module View where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           Model

view :: World -> IO Picture
view world = loadSprite

loadSprite :: IO Picture
loadSprite =
  loadJuicyPNG "sprites/player.png" >>=
  (\picture ->
     case picture of
       Just picture -> return picture
       Nothing -> undefined)
