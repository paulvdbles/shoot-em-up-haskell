-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model

import           Data.Aeson
import qualified Data.ByteString.Lazy             as BS

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Exit
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world = return $ updateBullets $ checkIfPlayerShouldBeMoved $ checkIfPlayerShouldShoot $ updateIteration world

updateIteration :: World -> World
updateIteration world = world {iteration = iteration world + 1}

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

calculateLeftCoordinate :: Player -> Coordinate
calculateLeftCoordinate player = calculateCoordinate player (-10) 0

calculateRightCoordinate :: Player -> Coordinate
calculateRightCoordinate player = calculateCoordinate player 10 0

calculateCoordinate :: Player -> Float -> Float -> Coordinate
calculateCoordinate player offsetX offsetY = Coordinate (oldX + offsetX) (oldY + offsetY)
  where
    playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
    oldX = x playerLocation
    oldY = y playerLocation

movePlayer :: Player -> Coordinate -> Player
movePlayer player newCoordinate =
  player {playerSpaceship = updateSpaceshipPositionInformation playerSpaceship' updatedPositionInformation}
  where
    playerSpaceship' = playerSpaceship player
    positionInformation = spaceshipPositionInformation playerSpaceship'
    updatedPositionInformation = updateLocation positionInformation newCoordinate

checkIfPlayerShouldShoot :: World -> World
checkIfPlayerShouldShoot world
  | shootKey (keyboard world) = shootBulletIfWeaponIsReloaded world
  | otherwise = world

shootBulletIfWeaponIsReloaded :: World -> World
shootBulletIfWeaponIsReloaded world
  | iteration' >= lastShotAtFrame weapon + reloadTime weapon = shootBullet world
  | otherwise = world
  where
    iteration' = iteration world
    playerSpaceship' = playerSpaceship (player world)
    weapon = head (filter active (weapons playerSpaceship'))

{- JSON STUFF -}
instance ToJSON Score where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Score

scoreFile :: FilePath
scoreFile = "scores.json"

getJSON :: IO BS.ByteString
getJSON = BS.readFile scoreFile

-- TODO add type here
writeJSON s = BS.writeFile scoreFile (encode s)

-- loadJSON :: Maybe JSON
loadJSON :: IO (Either String [Score])
loadJSON = eitherDecode <$> getJSON

shootBullet :: World -> World
shootBullet world =
  world
    { player = (player world) {playerSpaceship = (playerSpaceship (player world)) {weapons = updatedWeapons}}
    , bullets = updatedBullets
    }
  where
    playerSpaceship' = playerSpaceship (player world)
    weapon = head (filter active (weapons playerSpaceship'))
    spawnLocation = determineBulletsPositionInformation playerSpaceship'
    updatedBullets = (bullet weapon) {bulletPositionInformation = spawnLocation} : bullets world
    updatedWeapons = weapon {lastShotAtFrame = iteration world} : filter (not . active) (weapons playerSpaceship')

determineBulletsPositionInformation :: Spaceship -> PositionInformation
determineBulletsPositionInformation playerSpaceship = PositionInformation location' destination
  where
    location' = Coordinate (x playerLocation) (y playerLocation + 55)
    destination = Coordinate (x playerLocation) (y playerLocation + 1250) -- Add +1250 so the bullet's destination is outside the screen
    playerLocation = location (spaceshipPositionInformation playerSpaceship)
