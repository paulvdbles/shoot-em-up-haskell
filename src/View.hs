module View where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model

view :: World -> IO Picture
view world
  | state world == Playing = return (pictures (drawPlayer (player world) : drawBullets world))
  | state world == Menu = do
                          sc <- scores world
                          return (pictures ([drawPlayer (player world), translate (-220) 350 $ color red (text "Paused")] ++ [translate n m $ color red (text s) | m <- [100,100..], n <- [200,250..], s <- map scoreToString (getEither sc)]))
  where
    getEither (Right s) = s
    getEither (Left _) = [Score {playerName="Cannot load scores                  ", playerScore=0}]

scoreToString :: Score -> String
scoreToString Score {playerName=playerName, playerScore=playerScore} = playerName ++ replicate getStringLength ' ' ++ ps'
  where
    getStringLength | psl + pl > 10 = 10 - psl - pl
                    | otherwise = 1
    ps' :: String
    ps' = show playerScore
    pl = length playerName
    psl = length ps'

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