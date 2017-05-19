{-# LANGUAGE OverloadedStrings #-}
module Commands where


import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, string, space, decimal, many1, letter_ascii, sepBy, char, digit, digit)
import Data.Char (ord)
import Data.Text (Text, pack)


data Command
  = SettingsCommand Setting
  | UpdateCommand Update
  | ActionCommand Action
  | EmptyCommand
  deriving (Show)


data Setting
  = Timebank Int
  | TimePerMove Int
  | PlayerNames [Text]
  | YourBot Text
  | YourBotId Int
  | FieldWidth Int
  | FieldHeight Int
  deriving (Show)


data Update
  = GameRound Int
  | GameField [FieldItem] 
  deriving (Show)


data Action
  = Move Int
  deriving (Show)


data FieldItem
  = Empty
  | Wall
  | Player Int
  deriving (Show, Eq)


data MoveDirection
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | MovePass
  deriving (Eq, Ord)


instance Show MoveDirection where 
  show MoveLeft = "left"
  show MoveRight = "right"
  show MoveUp = "up"
  show MoveDown = "down"
  show MovePass = "no_moves"


command :: Parser Command
command =
  settingsCommand <|> updateCommand <|> actionCommand <|> emptyCommand
  where
    settingsCommand =
      SettingsCommand <$> (string "settings" *> space *> setting)
    updateCommand =
      UpdateCommand <$> (string "update" *> space *> update)
    actionCommand =
      ActionCommand <$> (string "action" *> space *> action)
    setting =
      timebank <|> timePerMove <|> playerNames <|> yourBot <|> yourBotId <|> fieldWidth <|> fieldHeight
    update =
      gameRound <|> gameField
    action =
      Move <$> (string "move" *> space *> decimal)
    timebank =
      Timebank <$> (string "timebank" *> space *> decimal)
    timePerMove =
      TimePerMove <$> (string "time_per_move" *> space *> decimal)
    playerNames =
      PlayerNames <$> (string "player_names" *> space *> playerNamesList)
    yourBot =
      YourBot <$> (string "your_bot" *> space *> playerName)
    yourBotId =
      YourBotId <$> (string "your_botid" *> space *> decimal)
    fieldWidth =
      FieldWidth <$> (string "field_width" *> space *> decimal)
    fieldHeight =
      FieldHeight <$> (string "field_height" *> space *> decimal) 
    gameRound =
      GameRound <$> (string "game round " *> decimal)
    gameField =
      GameField <$> (string "game field " *> (gameFieldItem `sepBy` char ',')) 
    playerNamesList =
      playerName `sepBy` char ','
    playerName =
      pack <$> many1 (letter_ascii <|> digit) 
    gameFieldItem =
      fiEmpty <|> fiWall <|> fiPlayer
    fiEmpty =
      char '.' *> return Empty
    fiWall =
      char 'x' *> return Wall
    fiPlayer =
      (Player . toInt) <$> digit 
    toInt c =
      ord c - ord '0'
    emptyCommand =
      string "" *> return EmptyCommand
