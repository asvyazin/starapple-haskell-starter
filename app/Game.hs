module Game where


import Commands (FieldItem)
import Data.Array (Array, listArray)
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
