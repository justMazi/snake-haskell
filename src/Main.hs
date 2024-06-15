module Main where

import Config
import GameLogic
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Input
import Rendering
import System.Random (randomIO)

main :: IO ()
main = do
  value <- randomIO
  play window windowBackground Config.fps (initialState True value 0) gameRender servicePressedKeys updateState
