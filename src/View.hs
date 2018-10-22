module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view world = render initialPlayer

-- We should use this but idk how yet

drawWorld :: World -> [IO Picture]
drawWorld world = [drawPlayer world, drawEnemies world, drawObstacles world, drawBullets world]

drawPlayer :: World -> IO Picture
drawPlayer world = render initialPlayer

drawEnemies :: World -> IO Picture
drawEnemies world = undefined

drawObstacles :: World -> IO Picture
drawObstacles world = undefined

drawBullets :: World -> IO Picture
drawBullets world = undefined