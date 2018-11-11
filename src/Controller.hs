module Controller where

import           Data.List
import           Enemy
import           FileSystem
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Movement
import           Shooting
import           System.Exit
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world
  | state world == Playing =
    return $
    addEnemies $
    moveEnemies $
    updateBullets $
    checkIfPlayerPauses $
    checkIfPlayerShouldBeMoved $
    updateShootingEnemies $
    checkIfPlayerShouldShoot $
    removeDeadEnemies $
    removeHitBullets $
    updatePlayerForAllEnemyBullets $
    updateEnemiesForAllBullets $ checkIfLevelDone $ checkIfPlayerDied $ updateIteration world
  | state world == Menu = return $ checkIfPlayerPauses world
  | state world == GameWin = return $ checkIfPlayerPauses world
  | state world == GameOver = return $ checkIfPlayerPauses world
  | state world == AskForUsername = return $ checkIfPlayerPauses world
  | state world == GameDone = newScoreList world
  | state world == Quitting = return world

updateIteration :: World -> World
updateIteration world = world {iteration = iteration world + 1}

-- | Appends new score to the current list, only keep the top 10 and sort it by score
newScoreList :: World -> IO World
newScoreList world = do
  sc <- scores world
  writeScoreFile (newScores sc)
  return $ world {scores = return (newScores sc), state = checkState}
  where
    player' = player world
    playerName' = username player'
    playerScore' = score player'
    playerHealth = health (playerSpaceship player')
    checkState
      | playerHealth <= 0 = GameOver
      | otherwise = GameWin
    newScores sc =
      take
        10
        (sortBy
           (\(Score p1 s1) (Score p2 s2) ->
              if s1 > s2
                then LT
                else GT)
           (Score playerName' playerScore' : sc))

checkIfPlayerPauses :: World -> World
checkIfPlayerPauses world
  | pauseKey (keyboard world) && state world == Playing = world {state = Menu}
  | enterKey (keyboard world) && state world == Menu = world {state = Playing}
  | enterKey (keyboard world) && state world == GameWin = world {state = Quitting}
  | enterKey (keyboard world) && state world == GameOver = world {state = Quitting}
  | enterKey (keyboard world) && state world == AskForUsername = world {state = Playing}
  | enterKey (keyboard world) && state world == AskForUsername = world {state = Playing}
  | otherwise = world

checkIfLevelDone :: World -> World
checkIfLevelDone world
  | levelTime - (iteration world `div` 60) <= 0 = world {state = GameDone}
  | otherwise = world

-- | Handle user input
input :: Event -> World -> IO World
input event world =
  case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> return $ world {keyboard = (keyboard world) {upKey = True}}
    EventKey (SpecialKey KeyUp) Up _ _ -> return $ world {keyboard = (keyboard world) {upKey = False}}
    EventKey (SpecialKey KeyDown) Down _ _ -> return $ world {keyboard = (keyboard world) {downKey = True}}
    EventKey (SpecialKey KeyDown) Up _ _ -> return $ world {keyboard = (keyboard world) {downKey = False}}
    EventKey (SpecialKey KeyLeft) Down _ _ -> return $ world {keyboard = (keyboard world) {leftKey = True}}
    EventKey (SpecialKey KeyLeft) Up _ _ -> return $ world {keyboard = (keyboard world) {leftKey = False}}
    EventKey (SpecialKey KeyRight) Down _ _ -> return $ world {keyboard = (keyboard world) {rightKey = True}}
    EventKey (SpecialKey KeyRight) Up _ _ -> return $ world {keyboard = (keyboard world) {rightKey = False}}
    EventKey (Char 'z') Down _ _ -> return $ world {keyboard = (keyboard world) {shootKey = True}}
    EventKey (Char 'z') Up _ _ -> return $ world {keyboard = (keyboard world) {shootKey = False}}
    EventKey (SpecialKey KeyEsc) Down _ _ -> return $ world {keyboard = (keyboard world) {pauseKey = True}}
    EventKey (SpecialKey KeyEsc) Up _ _ -> return $ world {keyboard = (keyboard world) {pauseKey = False}}
    EventKey (SpecialKey KeyEnter) Down _ _ -> return $ world {keyboard = (keyboard world) {enterKey = True}}
    EventKey (SpecialKey KeyEnter) Up _ _ -> return $ world {keyboard = (keyboard world) {enterKey = False}}
    EventKey (Char c) Down _ _ -> return $ world {player = (player world) {username = username (player world) ++ [c]}}
    EventResize newSize ->
      if newSize /= (720, 960)
        then exitSuccess -- sorry for this
        else return world
    _ -> return world
