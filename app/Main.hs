module Main where


import Commands (Command(..), Setting(..), Update(..), MoveDirection, command)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, runStateT)
import Data.Array (listArray)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B (toStrict, lines, getContents)
import Game (Game(..), GameSettings(..), GameState(..), emptyGame)
import Move (move)
import System.IO (hFlush, stdout)
import System.Random (RandomGen, mkStdGen)


seed :: Int
seed = 12345


main :: IO ()
main = do
  ls <- (map B.toStrict . B.lines) <$> B.getContents
  void $ runStateT (gameLoop emptyGame ls) $ mkStdGen seed
  where gameLoop _ [] = pure ()
        gameLoop game (l:ls) = do
          let res = parseOnly command l
          case res of
            Left err -> error err
            Right cmd -> do
              (newGame, maybeMove) <- processCommand game cmd
              case maybeMove of
                Just m -> do
                  liftIO $ print m
                  liftIO $ hFlush stdout
                _ -> pure ()
              gameLoop newGame ls


processCommand :: MonadState g m => RandomGen g => Game -> Command -> m (Game, Maybe MoveDirection)
processCommand game (SettingsCommand settingsCmd) = do
  let newSettings = processSettingsCommand (gameSettings game) settingsCmd
      newGame = game { gameSettings = newSettings }
  pure (newGame, Nothing)
  where processSettingsCommand settings (Timebank timebank) = settings { settingsTimeBank = timebank }
        processSettingsCommand settings (TimePerMove timePerMove) = settings { settingsTimePerMove = timePerMove }
        processSettingsCommand settings (PlayerNames playerNames) = settings { settingsPlayerNames = playerNames }
        processSettingsCommand settings (YourBot yourBot) = settings { settingsYourBot = yourBot }
        processSettingsCommand settings (YourBotId yourBotId) = settings { settingsYourBotId = yourBotId }
        processSettingsCommand settings (FieldWidth fieldWidth) = settings { settingsFieldWidth = fieldWidth }
        processSettingsCommand settings (FieldHeight fieldHeight) = settings { settingsFieldHeight = fieldHeight }
processCommand game (UpdateCommand updateCmd) = do
  let newState =
        processUpdateCommand (gameState game) updateCmd
      newGame =
        game { gameState = newState }
  pure (newGame, Nothing)
  where processUpdateCommand state (GameRound r) = state { stateRound = r }
        processUpdateCommand state (GameField field) =
          let settings = gameSettings game
              w = settingsFieldWidth settings
              h = settingsFieldHeight settings
          in state { stateField = listArray ((0, 0), (w - 1, h - 1)) field }
processCommand game (ActionCommand _) = do
  m <- move game
  pure (game, Just m)
processCommand game _ =
  pure (game, Nothing)
