module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view world
  | state world == Playing = return (pictures (drawPlayer (player world) : drawBullets world))
  | state world == Menu = return (pictures [drawPlayer (player world), translate (-250) 0 $ color red  (text "Paused")])

drawPlayer :: Player -> Picture
drawPlayer Player {playerSpaceship = spaceship} =
  translate (x playerPosition) (y playerPosition) $ color (light blue) $ rectangleSolid 50 80
  where
    playerPosition = location (spaceshipPositionInformation spaceship)

drawBullets :: World -> [Picture]
drawBullets  world = map drawBullet (bullets world)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (x bulletPosition) (y bulletPosition) $ color white $ circleSolid 10
  where
    bulletPosition = location (bulletPositionInformation bullet)