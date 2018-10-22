-- | This module defines how to turn
--   the game state into a picture

{-# LANGUAGE LambdaCase #-}

module View where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           Model

-- should update view

view :: World -> IO Picture
view world = render initialPlayer

