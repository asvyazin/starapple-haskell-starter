module Game where


import Commands (FieldItem(..), MoveDirection(..))
import Data.Array (Array, listArray, bounds, (!), assocs)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text, empty)
import System.Random (mkStdGen, RandomGen(..), StdGen)


data Game
     = Game
     { gameSettings :: GameSettings
     , gameState :: GameState
     , gameRnd :: StdGen
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


emptyGame :: Int -> Game
emptyGame seed = Game emptySettings emptyState $ mkStdGen seed
  where emptySettings = GameSettings 0 0 [] empty 0 0 0
        emptyState = GameState 0 emptyField
        emptyField = listArray ((0, 0), (0, 0)) []


instance RandomGen Game where
         next g =
           let (i, newRnd) = next (gameRnd g)
           in (i, g { gameRnd = newRnd })
         split g =
           let (rnd1, rnd2) = split (gameRnd g)
           in (g { gameRnd = rnd1 }, g { gameRnd = rnd2 })


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
