module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Juicy
import           Model

view :: World -> IO Picture
view World {player = player} = return (pictures [drawPlayer player])

drawPlayer :: Player -> Picture
drawPlayer Player {playerSpaceship = spaceship} =
  translate playerXPosition playerYPosition $ color (light blue) $ rectangleSolid 50 80
  where
    playerPosition = getLocation (getSpaceshipPosition spaceship)
    playerXPosition = getXCoordinate playerPosition
    playerYPosition = getYCoordinate playerPosition


