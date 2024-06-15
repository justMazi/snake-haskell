# snake-haskell

Final project for the Non-procedural programming course at MFF UK

<video width="320" height="240" controls>
  <source src="snake.mov" type="video/mp4">
</video>

### Potential setup pitfalls

In case you encounter the following error `Could not load module ‘Data.Map’
haskell`

If you use cabal, try running the following command:

```
cabal install containers
```

Then you should be able to import map with following line of code:

```
import Data.Map as Map
```

After you can compile the game using

```
ghc Main.hs
```

and run it using newly created executable file

# User Docs

After running the executable the game is started by pressing the space bar. You can turn the snake by pressing either WASD, or arrows. In order to boost the snake's movement, keep pressing spacebar.

Whenever you run into a wall or into the snake itself the game ends as in the original version of snake. You will then be shown your current best score. During the game you can see the score achieved so far during this run.

Game can be exited at any moment by pressing `esc`.

# Programming Docs

Since this is a game written in haskell, the main idea to the game logic is to essentialy have a function of time that changes game state based on current parameters - those parameters can be whether a spacebar is pressed or not or any other input/parameter.

For rendering of the user interface i have used the [Gloss package](https://hackage.haskell.org/package/gloss)

It is also used to run the very basic rendering loop and therefore we get a lot of boilerplate for free.

```
play window windowBackground Config.fps (initialState True value 0) gameRender servicePressedKeys updateState

```

in order to use the library we utilize its `play` function and provide it with all necessary things like the window, framerate, our own custom `GameState` (at this point its initial state, note that we start in a GameOver state) and a function to update the state of the game together with a rendering function. both of them are called by the library itself.

#### GameState

This is what the custom GameState looks like:

```
data GameState = GameState
  { getSnake :: Snake,
    getFood :: Food,
    getDirection :: Direction,
    isGameOver :: Bool,
    getRandomStdGen :: StdGen,
    wantedNewDirection :: Direction,
    getScore :: Score
  }

```

### Update State

The core logic of the game is in the `upateState` function. It gets us the following game state based on the current environment.

```
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
```

It takes care of calculating the position of snake, generating new food if the current one was eaten, checking whether game is over yet or not and it also holds the score for us.

### Config

Module config as the name suggests provides configuration for the game itself. I have extracted most of the values here to

1. Avoid `magic constants`
2. Allow for easy modification of some common settings like framerate, initial state as well as colors assigned to the game objects.

```
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

```

### Input

Input module once again utilizes the gloss library, this time for listening to input events. The following usage allows for listening to specific keys and buttons, and hooking the event up to a function call the ultimately updates the game state. this new game state is then picked up by the `play` function loop from before and already mentioned `updateState` function returns state with eg. changed direction.

```
servicePressedKeys (EventKey (Char 'a') Down _ _) gameState = setWantedDirection gameState LEFT
```

### Rendering

In the rendering module the `GameState` is rendered onto a canvas together with a the edges bounding the playground. It also conditionaly renders the game over text.

### Conclusion

This docs cover the main parts of the project, there is a bunch of other functions defined that I would say are mostly self explanatory or not as interestiong to be covered here.

Even though things like setting up the `Gloss` package were indeed a bit painful, ultimately i learned a lot while doing this little project and it had helped me practice haskell enough to pass the exam without any major issues.
I tried to structure the project in the cleanest way possible, but frankly having no production experience with haskell this is what i ended up with.
