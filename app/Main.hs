module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main =
  playIO
    (InWindow "shoot-em-up-haskell" (720, 960) (0, 0))
    black -- Background color
    10 -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function