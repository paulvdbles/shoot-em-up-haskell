-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Data.Text                        hiding (filter, foldr)
import           Data.Text.Internal               (showText)
import           Enemy
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.UI.TinyFileDialogs
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
    updateBullets $
    checkIfPlayerPauses $
    checkIfPlayerShouldBeMoved $
    updateShootingEnemies $
    checkIfPlayerShouldShoot $
    removeDeadEnemies $
    removeHitBullets $
    updatePlayerForAllEnemyBullets $ updateEnemiesForAllBullets $ checkIfLevelDone $ updateIteration world
  | state world == Menu = return $ checkIfPlayerPauses world
  | state world == GameWin = return $ checkIfPlayerPauses world
  | state world == AskForUsername = return $ world {player = player', state = Playing}
  where
    player' = (player world) {username = inputBox (pack "title") (pack "message") (Just (pack "username"))}

--checkUsername :: Monad m => m (Maybe Text) -> m Text
--checkUsername Nothing = return "Something went wrong - did you insert your name?"
--checkUsername (Just t) = return t

updateIteration :: World -> World
updateIteration world = world {iteration = iteration world + 1}

-- TODO check if we need to add extra LOC to go from GameOver/GameWin state to Menu or something
checkIfPlayerPauses :: World -> World
checkIfPlayerPauses world
  | pauseKey (keyboard world) && state world == Playing = world {state = Menu}
  | enterKey (keyboard world) && state world == Menu = world {state = Playing}
  | enterKey (keyboard world) && state world == GameWin = world {state = Quitting} --  TODO add winner input box?
  | otherwise = world

--  add an input window for the username
checkIfLevelDone :: World -> World
checkIfLevelDone world
  | levelTime - (iteration world `div` 60) <= 0 = world {state = GameWin}
  | otherwise = world

-- | Handle user input
input :: Event -> World -> IO World
input event world =
  case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> return (world {keyboard = (keyboard world) {upKey = True}})
    EventKey (SpecialKey KeyUp) Up _ _ -> return (world {keyboard = (keyboard world) {upKey = False}})
    EventKey (SpecialKey KeyDown) Down _ _ -> return (world {keyboard = (keyboard world) {downKey = True}})
    EventKey (SpecialKey KeyDown) Up _ _ -> return (world {keyboard = (keyboard world) {downKey = False}})
    EventKey (SpecialKey KeyLeft) Down _ _ -> return (world {keyboard = (keyboard world) {leftKey = True}})
    EventKey (SpecialKey KeyLeft) Up _ _ -> return (world {keyboard = (keyboard world) {leftKey = False}})
    EventKey (SpecialKey KeyRight) Down _ _ -> return (world {keyboard = (keyboard world) {rightKey = True}})
    EventKey (SpecialKey KeyRight) Up _ _ -> return (world {keyboard = (keyboard world) {rightKey = False}})
    EventKey (Char 'z') Down _ _ -> return (world {keyboard = (keyboard world) {shootKey = True}})
    EventKey (Char 'z') Up _ _ -> return (world {keyboard = (keyboard world) {shootKey = False}})
    EventKey (SpecialKey KeyEsc) Down _ _ -> return (world {keyboard = (keyboard world) {pauseKey = True}})
    EventKey (SpecialKey KeyEsc) Up _ _ -> return (world {keyboard = (keyboard world) {pauseKey = False}})
    EventKey (SpecialKey KeyEnter) Down _ _ -> return (world {keyboard = (keyboard world) {enterKey = True}})
    EventKey (SpecialKey KeyEnter) Up _ _ -> return (world {keyboard = (keyboard world) {enterKey = False}})
    EventResize newSize ->
      if newSize /= (720, 960)
        then exitSuccess -- sorry for this
        else return world
    _ -> return world

removeHitBullets :: World -> World
removeHitBullets world = world {bullets = filter (not . hit) (bullets world)}

removeDeadEnemies :: World -> World
removeDeadEnemies world = world {enemies = filter (\enemy -> health (enemySpaceship enemy) > 0) (enemies world)}

addEnemies :: World -> World
addEnemies world =
  world
    {enemies = enemies world ++ addEnemy spawns (iteration world), level = Level (removeSpawn spawns (iteration world))}
  where
    spawns = getSpawns (level world)
    getSpawns (Level xs) = xs

addEnemy :: [Spawn] -> Time -> [Enemy]
addEnemy xs currentTime = foldr addEnemy' [] xs
  where
    addEnemy' (Spawn (PlaceableEnemy e) spawnTime) acc
      | currentTime >= spawnTime = e : acc
      | otherwise = acc
    addEnemy' (Spawn _ spawnTime) acc = acc

-- remove a spawn from the spawnlist when its time has gone by
removeSpawn :: [Spawn] -> Time -> [Spawn]
removeSpawn xs currentTime = foldr removeSpawn' [] xs
  where
    removeSpawn' (Spawn x spawnTime) acc
      | currentTime > spawnTime = acc
      | otherwise = Spawn x spawnTime : acc
