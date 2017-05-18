module Move where


import Commands (MoveDirection(..))
import Game (Game(..), GameState(..), GameSettings(..), getNeighbours, getPlayerPosition)
import Random (chooseRandom)


move :: Game -> (MoveDirection, Game)
move game = let field = stateField $ gameState game
                yourBotId = settingsYourBotId $ gameSettings game
                yourPos = getPlayerPosition yourBotId field
                neighbours = getNeighbours field yourPos
                ((m, _), newGame) = chooseRandom game neighbours
            in (m, newGame)
