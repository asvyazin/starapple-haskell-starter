module Move where


import Control.Monad.State (MonadState)
import Commands (MoveDirection(..))
import Game (Game(..), GameState(..), GameSettings(..), getNeighbours, getPlayerPosition)
import Random (chooseRandomM)
import System.Random (RandomGen)


move :: MonadState g m => RandomGen g => Game -> m MoveDirection
move game = do
  let field =
        stateField $ gameState game
      yourBotId =
        settingsYourBotId $ gameSettings game
      yourPos =
        getPlayerPosition yourBotId field
      neighbours =
        getNeighbours field yourPos
  case neighbours of
    [] -> pure MovePass
    _ -> do
      (m, _) <- chooseRandomM neighbours
      pure m
