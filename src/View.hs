module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view World {player = player} =
  return (pictures [drawPlayer player, translate 0 0 $ color white $ circleSolid 10])

drawPlayer :: Player -> Picture
drawPlayer Player {playerSpaceship = spaceship} =
  translate (x playerPosition) (y playerPosition) $ color (light blue) $ rectangleSolid 50 80
  where
    playerPosition = location (spaceshipPositionInformation spaceship)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (x bulletPosition) (y bulletPosition) $ color white $ circleSolid 10
  where
    bulletPosition = location (bulletPositionInformation bullet)