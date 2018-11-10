module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view world
  | state world == Playing = return (pictures (drawPlayer (player world) : drawBullets world ++ drawEnemies world))
  | state world == Menu = do
    sc <- scores world
    print $ show (iteration world)
    return (pictures ([drawPaused, drawPressEnter] ++ drawPlayerNames sc ++ drawPlayerScores sc))
  where
    scoreToPlayerName Score {playerName = playerName} = playerName
    scoreToPlayerScore Score {playerScore = playerScore} = show playerScore
    drawPaused = translate (-220) 350 $ color red (text "Paused")
    drawPressEnter = translate (-210) 320 $ scale 0.2 0.2 $ color orange (text "Press Enter to continue")
    drawPlayerNames sc =
      [ translate x (fst ys) $ scale 0.2 0.2 $ color orange (text (snd ys))
      | x <- [-220]
      , ys <- zip [0,-25 .. -225] (map scoreToPlayerName sc)
      ]
    drawPlayerScores sc =
      [ translate x (fst ys) $ scale 0.2 0.2 $ color orange (text (snd ys))
      | x <- [200]
      , ys <- zip [0,-25 .. -225] (map scoreToPlayerScore sc)
      ]

drawPlayer :: Player -> Picture
drawPlayer Player {playerSpaceship = spaceship} =
  translate (x playerPosition) (y playerPosition) $ color (light blue) $ rectangleSolid 50 80
  where
    playerPosition = location (spaceshipPositionInformation spaceship)

drawBullets :: World -> [Picture]
drawBullets world = map drawBullet (bullets world)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (x bulletPosition) (y bulletPosition) $ color white $ circleSolid 10
  where
    bulletPosition = location (bulletPositionInformation bullet)

drawEnemies :: World -> [Picture]
drawEnemies world = map (drawEnemy (iteration world)) (enemies world)

drawEnemy ::Int -> Enemy -> Picture
drawEnemy currentIteration enemy = translate (x enemyPosition) (y enemyPosition) $ enemyColour $ rectangleSolid 40 40
  where enemyPosition = location (spaceshipPositionInformation (enemySpaceship enemy))
        enemyColour = if lastHitAtIteration enemy > currentIteration - 5
                          then color red -- make the enemy red for a few iterations
                          else color azure