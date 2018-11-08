module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Juicy
import           Model

view :: World -> Picture
view world = pictures [drawPlayer world]

drawPlayer :: World -> Picture
drawPlayer world = translate 0 (-200) $ color (light blue) $ rectangleSolid 50 80
