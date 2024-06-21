module GameLogic where

import Config
import Data.Map (Map, fromList, (!))
import Graphics.Gloss
import System.Random

type Score = Int

type Food = (Int, Int)

type Snake = [Food]

data Direction = UP | DOWN | LEFT | RIGHT | NOT deriving (Eq, Ord)

data GameState = GameState
  { getSnake :: Snake,
    getFood :: Food,
    getDirection :: Direction,
    isGameOver :: Bool,
    getRandomStdGen :: StdGen,
    wantedNewDirection :: Direction,
    getScore :: Score
  }

-- Mapping directions to their respective vectors
directionVectors :: Map Direction (Int, Int)
directionVectors = fromList [(UP, (0, -1)), (DOWN, (0, 1)), (LEFT, (-1, 0)), (RIGHT, (1, 0))]

-- Set the desired direction of the snake
setWantedDirection :: GameState -> Direction -> GameState
setWantedDirection (GameState snake food direction game random newDirection score) wantedDirection =
  GameState snake food direction game random wantedDirection score

-- Initialize the game state with initial values
initialState :: Bool -> Int -> Score -> GameState
initialState gameOver seed score =
  GameState
    { getSnake = [snake],
      getFood = food,
      getDirection = RIGHT,
      isGameOver = gameOver,
      getRandomStdGen = mkStdGen seed,
      wantedNewDirection = NOT,
      getScore = score
    }
  where
    snake = Config.initialSnakePosition
    food = Config.initialFoodPosition

-- Change the direction of the snake based on user input, ensuring it doesn't reverse direction
changeDirection :: GameState -> GameState
changeDirection state@(GameState snake food direction game random newDirection score) =
  if (fst vector1 + fst vector2 == 0 && snd vector1 + snd vector2 == 0)
    then state
    else GameState snake food newDirection game random NOT score
  where
    vector1 = directionVectors ! direction
    vector2 = directionVectors ! newDirection

-- Boost the speed of the snake by updating the game state more frequently
boostDirection :: GameState -> GameState
boostDirection gameState = updateState Config.boostValue gameState

-- Check if the game is over by verifying if the snake collides with the border or itself
checkGameOver :: Snake -> Bool
checkGameOver snake = headX == 0 || headX == 32 || headY == 0 || headY == 24 || headSnake `elem` body
  where
    headSnake = head snake
    (headX, headY) = headSnake
    body = tail snake

-- Move the snake in the specified direction, checking if food is eaten and updating the snake's position accordingly
movePlayer :: Food -> Direction -> Snake -> (Bool, Snake)
movePlayer food direction snake
  | foodEaten = (True, newHead : snake)
  | otherwise = (False, newHead : init snake)
  where
    foodEaten = food == newHead
    newHead = (headX + shiftX, headY + shiftY)
    (shiftX, shiftY) = directionVectors ! direction
    (headX, headY) = head snake

-- Update the game state for each frame, including moving the snake, checking for collisions, and generating new food
updateState :: Float -> GameState -> GameState
updateState _ gameState =
  if gameOver
    then gameState
    else
      if newDir == NOT
        then state
        else changeDirection state
  where
    state = GameState newSnake newFood direction newGameOver newStdGen newDir newScore
    snake = getSnake gameState
    food = getFood gameState
    direction = getDirection gameState
    gameOver = isGameOver gameState
    stdGen = getRandomStdGen gameState
    newDir = wantedNewDirection gameState
    score = getScore gameState

    newScore =
      if length newSnake > score
        then score + 1
        else score
    (foodEaten, newSnake) = movePlayer food direction snake
    (newFood, newStdGen) =
      if foodEaten
        then generateNewFood newSnake stdGen
        else (food, stdGen)
    newGameOver = checkGameOver newSnake

-- Generate a new food position, ensuring it does not overlap with the snake
generateNewFood :: Snake -> StdGen -> (Food, StdGen)
generateNewFood snake stdGen =
  if newFood `elem` snake
    then generateNewFood snake stdGen3
    else (newFood, stdGen3)
  where
    (foodX, stdGen2) = randomR (1, 31) stdGen
    (foodY, stdGen3) = randomR (1, 23) stdGen2
    newFood = (foodX, foodY)
