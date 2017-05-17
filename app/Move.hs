module Move where


import Commands (MoveDirection(..))
import Game (Game)


move :: Game -> (MoveDirection, Game)
move game = (MoveUp, game)
