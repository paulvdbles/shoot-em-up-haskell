module Levels where

import           Model

weapon = Weapon (Bullet 10 False initialPosition) True 0 10

initialPosition = PositionInformation (Coordinate 0 (-200)) (Coordinate 0 (-200))

level =
  Level
    [ Spawn
        (PlaceableSpaceship (Spaceship 1 100 [weapon] (PositionInformation (Coordinate 0 300) (Coordinate 0 (-200)))))
        0
    ]

newSpaceShip :: Float -> Float -> Spawn
newSpaceShip x y =
  Spawn
    (PlaceableSpaceship (Spaceship 1 10 [weapon] (PositionInformation (Coordinate x y) (Coordinate 0 0))))
    0
