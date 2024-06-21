module Input where

import GameLogic
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Handle user input events and update the game state accordingly
servicePressedKeys :: Event -> GameState -> GameState
servicePressedKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = setWantedDirection gameState LEFT
servicePressedKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = setWantedDirection gameState RIGHT
servicePressedKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = setWantedDirection gameState UP
servicePressedKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = setWantedDirection gameState DOWN
servicePressedKeys (EventKey (Char 'a') Down _ _) gameState = setWantedDirection gameState LEFT
servicePressedKeys (EventKey (Char 'd') Down _ _) gameState = setWantedDirection gameState RIGHT
servicePressedKeys (EventKey (Char 'w') Down _ _) gameState = setWantedDirection gameState UP
servicePressedKeys (EventKey (Char 's') Down _ _) gameState = setWantedDirection gameState DOWN

-- Handle SPACE key press for restarting the game or boosting direction
servicePressedKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState =
  if isGameOver gameState
    then initState
    else boostDirection gameState
  where
    (newSeed, _) = random (getRandomStdGen gameState)
    initState = initialState False newSeed $ getScore gameState
servicePressedKeys _ gameState = gameState
