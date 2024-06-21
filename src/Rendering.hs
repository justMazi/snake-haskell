module Rendering where

import Config qualified
import GameLogic
import Graphics.Gloss


-- Function to render the game state
-- This function converts the game state into a series of pictures to be drawn on the screen
gameRender :: GameState -> Picture
gameRender gameState =
  pictures $
    -- Render border
    [ fillRectangleBy black (16, 0) (660, 20),
      fillRectangleBy black (16, 24) (660, 20),
      fillRectangleBy black (0, 12) (20, 470),
      fillRectangleBy black (32, 12) (20, 470)
    ]
      -- Render snake, snake head, and food
      ++ fmap (convertToPicture chartreuse) snake
      ++ fmap (convertToPicture Config.snakeHeadColor) [snakeHead]
      ++ fmap (convertToPicture Config.foodColor) [food]
      ++ gameOverPicture
  where
    snake = getSnake gameState
    food = getFood gameState
    score = getScore gameState
    snakeHead = head snake

    -- Convert game objects to pictures
    convertToPicture :: Color -> (Int, Int) -> Picture
    convertToPicture color' (x, y) = fillRectangleBy color' (x, y) (20, 20)

    -- Create a filled rectangle picture
    fillRectangleBy :: Color -> (Int, Int) -> (Float, Float) -> Picture
    fillRectangleBy color' (x, y) (width, height) = color color' $ scale 1 (-1) $ translate (fromIntegral x * 20 - 320) (fromIntegral y * 20 - 240) $ rectangleSolid width height

    -- Display game over message if the game is over
    gameOverPicture =
      if isGameOver gameState
        then
          [ color black $ translate (-130) 180 $ scale 0.25 0.25 $ text $ "Best score : " ++ show score,
            color black $ translate (-155) 0 $ scale 0.4 0.4 $ text "GAME OVER",
            color black $ translate (-140) (-50) $ scale 0.2 0.2 $ text "Press SPACE to start."
          ]
        else
          [ color black $ translate (-40) 260 $ scale 0.15 0.15 $ text $ "Score: " ++ show (length snake)
          ]
