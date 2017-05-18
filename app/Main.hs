module Main where


import Commands (Command(..), Setting(..), Update(..), MoveDirection, command)
import Data.Array (listArray)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B (toStrict, lines, getContents)
import Game (Game(..), GameSettings(..), GameState(..), emptyGame)
import Move (move)
import System.IO (hFlush, stdout)


seed :: Int
seed = 12345


main :: IO ()
main = do
  ls <- (map B.toStrict . B.lines) <$> B.getContents
  gameLoop (emptyGame seed) ls
  where gameLoop _ [] = pure ()
        gameLoop game (l:ls) = do
          let res = parseOnly command l
          case res of
            Left err -> error err
            Right cmd -> do
              let (newGame, maybeMove) = processCommand game cmd
              case maybeMove of
                Just m -> do
                  print m
                  hFlush stdout
                _ -> pure ()
              gameLoop newGame ls


processCommand :: Game -> Command -> (Game, Maybe MoveDirection)
processCommand game (SettingsCommand settingsCmd) =
  let newSettings = processSettingsCommand (gameSettings game) settingsCmd
      newGame = game { gameSettings = newSettings }
  in (newGame, Nothing)
  where processSettingsCommand settings (Timebank timebank) = settings { settingsTimeBank = timebank }
        processSettingsCommand settings (TimePerMove timePerMove) = settings { settingsTimePerMove = timePerMove }
        processSettingsCommand settings (PlayerNames playerNames) = settings { settingsPlayerNames = playerNames }
        processSettingsCommand settings (YourBot yourBot) = settings { settingsYourBot = yourBot }
        processSettingsCommand settings (YourBotId yourBotId) = settings { settingsYourBotId = yourBotId }
        processSettingsCommand settings (FieldWidth fieldWidth) = settings { settingsFieldWidth = fieldWidth }
        processSettingsCommand settings (FieldHeight fieldHeight) = settings { settingsFieldHeight = fieldHeight }
processCommand game (UpdateCommand updateCmd) =
  let newState = processUpdateCommand (gameState game) updateCmd
      newGame = game { gameState = newState }
  in (newGame, Nothing)
  where processUpdateCommand state (GameRound r) = state { stateRound = r }
        processUpdateCommand state (GameField field) =
          let settings = gameSettings game
              w = settingsFieldWidth settings
              h = settingsFieldHeight settings
          in state { stateField = listArray ((0, 0), (w - 1, h - 1)) field }
processCommand game (ActionCommand _) =
  let (m, newGame) = move game
  in (newGame, Just m)
processCommand game _ = (game, Nothing)
