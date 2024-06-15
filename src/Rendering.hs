module Rendering where

import Config qualified
import GameLogic
import Graphics.Gloss

gameRender :: GameState -> Picture
gameRender gameState =
  pictures $
    [ fillRectangleBy black (16, 0) (660, 20),
      fillRectangleBy black (16, 24) (660, 20),
      fillRectangleBy black (0, 12) (20, 470),
      fillRectangleBy black (32, 12) (20, 470)
    ]
      ++ fmap (convertToPicture chartreuse) snake
      ++ fmap (convertToPicture Config.snakeHeadColor) [snakeHead]
      ++ fmap (convertToPicture Config.foodColor) [food]
      ++ gameOverPicture
  where
    snake = getSnake gameState
    food = getFood gameState
    score = getScore gameState
    snakeHead = head snake

    convertToPicture :: Color -> (Int, Int) -> Picture
    convertToPicture color' (x, y) = fillRectangleBy color' (x, y) (20, 20)

    fillRectangleBy :: Color -> (Int, Int) -> (Float, Float) -> Picture
    fillRectangleBy color' (x, y) (width, height) = color color' $ scale 1 (-1) $ translate (fromIntegral x * 20 - 320) (fromIntegral y * 20 - 240) $ rectangleSolid width height

    gameOverPicture =
      if isGameOver gameState
        then
          [ color aquamarine $ translate (-130) 180 $ scale 0.25 0.25 $ text $ "Best score : " ++ show score,
            color red $ translate (-155) 0 $ scale 0.4 0.4 $ text "GAME OVER",
            color blue $ translate (-140) (-50) $ scale 0.2 0.2 $ text "Press SPACE to start."
          ]
        else
          [ color blue $ translate (-40) 260 $ scale 0.15 0.15 $ text $ "Score: " ++ show (length snake)
          ]
