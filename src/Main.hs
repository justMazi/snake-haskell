module Main where

import Config
import GameLogic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Input
import Rendering
import System.Random (randomIO)


-- Entry point of the application
main :: IO ()
main = do
  value <- randomIO
  -- Initialize and start the game loop
  -- Arguments: window settings, background color, frame rate, initial game state, render function, input handling function, state updating function
  play window windowBackground Config.fps (initialState True value 0) gameRender servicePressedKeys updateState
