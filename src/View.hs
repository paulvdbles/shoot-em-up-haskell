module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view world
  | state world == Playing =
    return
      (pictures
         (drawPlayer (iteration world) (player world) :
          drawHealth (player world) : drawTime : drawScore : drawBullets world ++ drawEnemies world))
  | state world == Menu = do
    sc <- scores world
    return (pictures (drawPaused : drawPressEnter : (drawPlayerNames sc ++ drawPlayerScores sc)))
  | state world == GameWin = do
    sc <- scores world
    pn <- username (player world)
    return (pictures [drawPlayerScore, drawPlayerName pn, drawGameWinMessage])
  | state world == GameOver = do
    sc <- scores world
    pn <- username (player world)
    return (pictures [drawPlayerScore, drawPlayerName pn, drawGameOverMessage])
  where
    scoreToPlayerName Score {playerName = playerName} = playerName
    scoreToPlayerScore Score {playerScore = playerScore} = show playerScore
    drawPaused = translate (-220) 350 $ color red (text "Paused")
    drawPressEnter = translate (-210) 320 $ scale 0.2 0.2 $ color orange $ text "Press Enter to continue"
    drawTime = translate 300 450 $ scale 0.2 0.2 $ color red $ text $ show $ levelTime - (iteration world `div` 60)
    drawScore = translate 300 420 $ scale 0.2 0.2 $ color red $ text $ show $ score (player world)
    drawPlayerNames sc =
      [ translate x (fst ys) $ scale 0.2 0.2 $ color orange $ text $ snd ys
      | x <- [-220]
      , ys <- zip [0,-25 .. -225] (map scoreToPlayerName sc)
      ]
    drawPlayerScores sc =
      [ translate x (fst ys) $ scale 0.2 0.2 $ color orange $ text $ snd ys
      | x <- [200]
      , ys <- zip [0,-25 .. -225] (map scoreToPlayerScore sc)
      ]
    drawPlayerScore = translate (-50) 200 $ scale 0.2 0.2 $ color red $ text $ "Score: " ++ show (score $ player world)
    drawPlayerName pn =
      translate (-50) 150 $ scale 0.2 0.2 $ color red $ text $ "Score: " ++ show pn
    drawGameWinMessage = translate (-200) 200 $ scale 0.5 0.5 $ color red $ text "Winner winner!"
    drawGameOverMessage = translate (-300) 200 $ scale 0.4 0.4 $ color red $ text "Better luck next time!"

drawPlayer :: Int -> Player -> Picture
drawPlayer currentIteration Player {playerSpaceship = spaceship} =
  translate (x playerPosition) (y playerPosition) $ playerColour $ rectangleSolid 50 80
  where
    playerPosition = location (spaceshipPositionInformation spaceship)
    playerColour =
      if lastHitAtIteration spaceship > currentIteration - 5
        then color red -- make the enemy red for a few iterations
        else color (light blue)

drawBullets :: World -> [Picture]
drawBullets world = map drawBullet (bullets world)

drawBullet :: Bullet -> Picture
drawBullet bullet = translate (x bulletPosition) (y bulletPosition) $ color white $ circleSolid 10
  where
    bulletPosition = location (bulletPositionInformation bullet)

drawEnemies :: World -> [Picture]
drawEnemies world = map (drawEnemy (iteration world)) (enemies world)

drawEnemy :: Int -> Enemy -> Picture
drawEnemy currentIteration enemy = translate (x enemyPosition) (y enemyPosition) $ enemyColour $ rectangleSolid 40 40
  where
    enemyPosition = location (spaceshipPositionInformation (enemySpaceship enemy))
    enemyColour =
      if lastHitAtIteration (enemySpaceship enemy) > currentIteration - 5
        then color red -- make the enemy red for a few iterations
        else color azure

drawHealth :: Player -> Picture
drawHealth player = translate (-360) 460 $ color red $ rectangleSolid health' 30
  where
    health' = health (playerSpaceship player) * 2
