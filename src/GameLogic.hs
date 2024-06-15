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

directionVectors :: Map Direction (Int, Int)
directionVectors = fromList [(UP, (0, -1)), (DOWN, (0, 1)), (LEFT, (-1, 0)), (RIGHT, (1, 0))]

setWantedDirection :: GameState -> Direction -> GameState
setWantedDirection (GameState snake food direction game random newDirection score) wantedDirection =
  GameState snake food direction game random wantedDirection score

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

changeDirection :: GameState -> GameState
changeDirection state@(GameState snake food direction game random newDirection score) =
  if (fst vector1 + fst vector2 == 0 && snd vector1 + snd vector2 == 0)
    then state
    else GameState snake food newDirection game random NOT score
  where
    vector1 = directionVectors ! direction
    vector2 = directionVectors ! newDirection

boostDirection :: GameState -> GameState
boostDirection gameState = updateState Config.boostValue gameState

checkGameOver :: Snake -> Bool
checkGameOver snake = headX == 0 || headX == 32 || headY == 0 || headY == 24 || headSnake `elem` body
  where
    headSnake = head snake
    (headX, headY) = headSnake
    body = tail snake

movePlayer :: Food -> Direction -> Snake -> (Bool, Snake)
movePlayer food direction snake
  | foodEaten = (True, newHead : snake)
  | otherwise = (False, newHead : init snake)
  where
    foodEaten = food == newHead
    newHead = (headX + shiftX, headY + shiftY)
    (shiftX, shiftY) = directionVectors ! direction
    (headX, headY) = head snake

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

generateNewFood :: Snake -> StdGen -> (Food, StdGen)
generateNewFood snake stdGen =
  if newFood `elem` snake
    then generateNewFood snake stdGen3
    else (newFood, stdGen3)
  where
    (foodX, stdGen2) = randomR (1, 31) stdGen
    (foodY, stdGen3) = randomR (1, 23) stdGen2
    newFood = (foodX, foodY)
