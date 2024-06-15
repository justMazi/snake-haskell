module Config where

import Graphics.Gloss

-- window name, window size, offset from top left corner
window :: Display
window = InWindow "Snake Game" (900, 700) (250, 50)

windowBackground :: Color
windowBackground = white

fps :: Int
fps = 8

initialSnakePosition :: (Int, Int)
initialSnakePosition = (8, 6)

initialFoodPosition :: (Int, Int)
initialFoodPosition = (16, 12)

foodColor :: Color
foodColor = green

snakeHeadColor :: Color
snakeHeadColor = orange

boostValue :: Float
boostValue = 2