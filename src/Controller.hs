-- | This module defines how the state changes
--   in response to time and user input
module Controller where

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
    updateBullets $
    checkIfPlayerPauses $
    checkIfPlayerShouldBeMoved $
    checkIfPlayerShouldShoot $ removeDeadEnemies $ removeHitBullets $ updateEnemiesForAllBullets $ updateIteration world
  | state world == Menu = return $ checkIfPlayerPauses world

updateIteration :: World -> World
updateIteration world = world {iteration = iteration world + 1}

-- TODO check if we need to add extra LOC to go from GameOver/GameWin state to Menu or something
checkIfPlayerPauses :: World -> World
checkIfPlayerPauses world
  | pauseKey (keyboard world) && state world == Playing = world {state = Menu}
  | enterKey (keyboard world) && state world == Menu = world {state = Playing}
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
removeDeadEnemies world = world {enemies = filter enemyIsDead (enemies world)}

enemyIsDead :: Enemy -> Bool
enemyIsDead enemy = health (enemySpaceship enemy) > 0

addEnemies :: World -> World
addEnemies world = world {enemies = enemies world ++ addEnemy spawns (iteration world)}
  where
    spawns = getSpawns (level world)
    getSpawns (Level xs) = xs

addEnemy :: [Spawn] -> Time -> [Enemy]
addEnemy xs t = map (addEnemy' t) xs
  where addEnemy' currentTime (Spawn (PlaceableSpaceship e) spawnTime) | currentTime == spawnTime = Enemy 10 10 e (-1)
