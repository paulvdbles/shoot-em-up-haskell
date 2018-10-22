module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Graphics.Gloss.Juicy
import           Model

view :: World -> IO Picture
view world = undefined

-- We should use this but idk how yet
loadImages :: IO Images
loadImages = do
  Just playerImage' <- loadJuicyPNG "sprites/player.png"
  Just enemyImage' <- loadJuicyPNG "sprites/enemy.png"
  Just obstacleImage' <- loadJuicyPNG "sprites/obstacle.png"
  return Images {playerImage = playerImage', enemyImage = enemyImage', obstacleImage = obstacleImage'}

drawWorld :: World -> [IO Picture]
drawWorld world = [drawPlayer world, drawEnemies world, drawObstacles world, drawBullets world]

drawPlayer :: World -> IO Picture
drawPlayer world = undefined

drawEnemies :: World -> IO Picture
drawEnemies world = undefined

drawObstacles :: World -> IO Picture
drawObstacles world = undefined

drawBullets :: World -> IO Picture
drawBullets world = undefined
