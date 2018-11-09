-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

-- | Handle one iteration of the game
step :: Float -> World -> IO World
step secs world = return (checkIfPlayerShouldBeMoved world)

checkIfPlayerShouldBeMoved :: World -> World
checkIfPlayerShouldBeMoved world
  | upKey (keyboard world) = world {player = movePlayer (player world) (calculateUpCoordinate (player world))}
  | downKey (keyboard world) = world {player = movePlayer (player world) (calculateDownCoordinate (player world))}
  | leftKey (keyboard world) = world {player = movePlayer (player world) (calculateLeftCoordinate (player world))}
  | rightKey (keyboard world) =  world {player = movePlayer (player world) (calculateRightCoordinate (player world))}
  | otherwise = world

checkIfPlayerShouldShoot :: World -> World
checkIfPlayerShouldShoot world
  | shootKey (keyboard world) = undefined

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
    _ -> return world


calculateUpCoordinate :: Player -> Coordinate
calculateUpCoordinate player = Coordinate oldX (oldY + 10)
  where playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
        oldX = x playerLocation
        oldY = y playerLocation

calculateDownCoordinate :: Player -> Coordinate
calculateDownCoordinate player = Coordinate oldX (oldY - 10)
  where playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
        oldX = x playerLocation
        oldY = y playerLocation

calculateLeftCoordinate :: Player -> Coordinate
calculateLeftCoordinate player = Coordinate (oldX -10) oldY
  where playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
        oldX = x playerLocation
        oldY = y playerLocation

calculateRightCoordinate :: Player -> Coordinate
calculateRightCoordinate player = Coordinate (oldX +10) oldY
  where playerLocation = location (spaceshipPositionInformation (playerSpaceship player))
        oldX = x playerLocation
        oldY = y playerLocation

movePlayer :: Player -> Coordinate -> Player
movePlayer player newCoordinate =
  player {playerSpaceship = updateSpaceshipPositionInformation playerSpaceship' updatedPositionInformation}
  where
    playerSpaceship' = playerSpaceship player
    positionInformation = spaceshipPositionInformation playerSpaceship'
    updatedPositionInformation = updateLocation positionInformation newCoordinate


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

{-  END JSON STUFF -}