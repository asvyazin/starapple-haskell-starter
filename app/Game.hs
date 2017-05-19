module Game where


import Commands (FieldItem(..), MoveDirection(..))
import Data.Array (Array, listArray, bounds, (!), assocs)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text, empty)


data Game
     = Game
     { gameSettings :: GameSettings
     , gameState :: GameState
     } deriving (Show)


data GameSettings
     = GameSettings
     { settingsTimeBank :: Int
     , settingsTimePerMove :: Int
     , settingsPlayerNames :: [Text]
     , settingsYourBot :: Text
     , settingsYourBotId :: Int
     , settingsFieldWidth :: Int
     , settingsFieldHeight :: Int
     } deriving (Show)


type Point = (Int, Int)


type Field = Array Point FieldItem


data GameState
     = GameState
     { stateRound :: Int
     , stateField :: Field
     } deriving (Show)


emptyGame :: Game
emptyGame = Game emptySettings emptyState
  where emptySettings = GameSettings 0 0 [] empty 0 0 0
        emptyState = GameState 0 emptyField
        emptyField = listArray ((0, 0), (0, 0)) []


getNeighbours :: Field -> (Int, Int) -> [(MoveDirection, (Int, Int))]
getNeighbours g (y, x) =
  let
    possibleNeighbours =
      [ (MoveUp, (y - 1, x))
      , (MoveLeft, (y, x - 1))
      , (MoveDown, (y + 1, x))
      , (MoveRight, (y, x + 1))
      ]
  in
    filter (isValidPosition . snd) possibleNeighbours
  where
    isValidPosition p@(y', x') =
      let
        (_, (maxY, maxX)) =
          bounds g
        w =
          maxX + 1
        h =
          maxY + 1
      in
        (x' >= 0) && (x' < w) && (y' >= 0) && (y' < h) && isAccessiblePosition g p


isAccessiblePosition :: Field -> (Int, Int) -> Bool
isAccessiblePosition f =
  not . checkField f (\fi -> fi == Wall || isPlayer fi)


checkField :: Array (Int, Int) a -> (a -> Bool) -> (Int, Int) -> Bool
checkField field check pos =
  check $ field ! pos


isPlayer :: FieldItem -> Bool
isPlayer (Player _) = True
isPlayer _ = False


getPlayerPosition :: Int -> Field -> Point
getPlayerPosition playerId f =
  fst $ fromJust $ find ((== (Player playerId)) . snd) $ assocs f
