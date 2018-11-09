-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Exit
import           System.Random
import Movement
import Shooting

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world
  | state world == Playing = return $ updateBullets $ checkIfPlayerPauses $ checkIfPlayerShouldBeMoved $ checkIfPlayerShouldShoot $ updateIteration world
  | state world == Menu = return $ checkIfPlayerPauses world

updateIteration :: World -> World
updateIteration world = world {iteration = iteration world + 1}

-- TODO check if we need to add extra LOC to go from GameOver/GameWin state to Menu or something
checkIfPlayerPauses :: World -> World
checkIfPlayerPauses world
  | pauseKey (keyboard world) && state world == Playing = world {state = Menu}
  | pauseKey (keyboard world) && state world == Menu = world {state = Playing}
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
    EventResize newSize ->
      if newSize /= (720, 960)
        then exitSuccess -- sorry for this
        else return world
    _ -> return world

checkIfPlayerShouldBeMoved :: World -> World
checkIfPlayerShouldBeMoved world
  | upKey (keyboard world) = world {player = movePlayer (player world) (calculateUpCoordinate (player world))}
  | downKey (keyboard world) = world {player = movePlayer (player world) (calculateDownCoordinate (player world))}
  | leftKey (keyboard world) = world {player = movePlayer (player world) (calculateLeftCoordinate (player world))}
  | rightKey (keyboard world) = world {player = movePlayer (player world) (calculateRightCoordinate (player world))}
  | otherwise = world

updateBullets :: World -> World
updateBullets world = world {bullets = map moveBulletToDestination (removeOldBullets (bullets world))}

removeOldBullets :: [Bullet] -> [Bullet]
removeOldBullets = filter bulletShouldBeKept

bulletShouldBeKept :: Bullet -> Bool
bulletShouldBeKept bullet =
  y (location (bulletPositionInformation bullet)) <= y (destination (bulletPositionInformation bullet))

moveBulletToDestination :: Bullet -> Bullet
moveBulletToDestination bullet = bullet {bulletPositionInformation = updatedPositionInformation}
  where
    currentLocation = location (bulletPositionInformation bullet)
    newLocation = Coordinate (x currentLocation) (y currentLocation + 10)
    destination' = destination (bulletPositionInformation bullet)
    updatedPositionInformation = PositionInformation newLocation destination'

calculateUpCoordinate :: Player -> Coordinate
calculateUpCoordinate player = calculateCoordinate player 0 10

calculateDownCoordinate :: Player -> Coordinate
calculateDownCoordinate player = calculateCoordinate player 0 (-10)

