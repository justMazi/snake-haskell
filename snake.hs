import Data.Map

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
