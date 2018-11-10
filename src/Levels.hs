module Levels where

import           Model

weapon = Weapon (StraightBullet 10 False initialPosition True) True 0 10

initialPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

seconds :: Time -> Time
seconds n = n * 60

level =
  Level
    [ Spawn (newSpaceShip 0 300) (seconds 1)
    , Spawn (newSpaceShip 100 400) (seconds 2)
    , Spawn (newSpaceShip (-100) 200) (seconds 3)
    , Spawn (newSpaceShip 50 300) (seconds 4)
    ]

newSpaceShip :: Float -> Float -> Placeable
newSpaceShip x y = PlaceableSpaceship (Spaceship 1 100 [weapon] (PositionInformation (Coordinate x y) (Coordinate 0 0)))
