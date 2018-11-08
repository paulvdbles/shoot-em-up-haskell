module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Juicy
import           Model

view :: World -> IO Picture
view world = return (pictures [drawPlayer world])

drawPlayer :: World -> Picture
drawPlayer world = translate playerXPosition playerYPosition $ color (light blue) $ rectangleSolid 50 80
  where
    playerXPosition = 0
    playerYPosition = -200
