module Main (main) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List
import System.IO

data Label = A | B | C deriving (Eq, Ord, Show, Read, Enum)
type Coord = (Label, Label)
type Board = Map Coord (Maybe Player)
data Player = X | O deriving (Eq, Ord, Show)
data GameState = GameState Player Board
data GameResult = Win Player | Tie deriving (Eq, Ord, Show)

validCoords :: [Coord]
validCoords = [(x,y) | x <- [A .. C], y <- [A .. C]]

-- This should display a nice ASCII representation of the game state like:
--  X| |
--  -+-+-
--   |O|
--  -+-+-
--   | |
-- It is X's turn.
instance Show GameState where
  show (GameState player board) = intercalate " -+-+-\n"
                                  (fmap showRow
                                  [[(x,y) | x <- [A .. C]] | y <- [A .. C]])
                                  ++ "It is " ++ show player ++ "'s turn.\n"
    where
      showSpace :: Coord -> String
      showSpace coord = case board Map.! coord of
                        Nothing -> " "
                        Just x -> show x
      showRow :: [Coord] -> String
      showRow row = " " ++ intercalate "|" (fmap showSpace row) ++ "\n"

emptyBoard :: Board
emptyBoard = Map.fromList (zip validCoords (repeat Nothing))

startState :: GameState
startState = GameState X emptyBoard

straightLines :: [[Coord]]
straightLines = [
              [(A,A), (A,B), (A,C)],
              [(B,A), (B,B), (B,C)],
              [(C,A), (C,B), (C,C)],
              [(A,A), (B,A), (C,A)],
              [(A,B), (B,B), (C,B)],
              [(A,C), (B,C), (C,C)],
              [(A,A), (B,B), (C,C)],
              [(A,C), (B,B), (C,A)]]

isWinningLine :: [Maybe Player] -> Maybe Player
isWinningLine [Just x, Just y, Just z] | x == y && y == z = Just x
isWinningLine _ = Nothing

getGameResult :: GameState -> Maybe GameResult
getGameResult (GameState _ board) = case winningLines of
                                    (x:_) -> Just $ Win $ x
                                    [] -> if notElem Nothing $ Map.elems board
                                          then Just Tie else Nothing
  where
    winningLines :: [Player]
    winningLines = catMaybes $ fmap isWinningLine
                             $ fmap (fmap (board Map.!))
                             straightLines

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    (a, "") : _ -> Just a
    _ -> Nothing

getMove :: GameState -> IO Coord
getMove state@(GameState _ board) = do
          putStr " > "; hFlush stdout
          input <- getLine
          let coord = readMaybe ("(" ++ input ++ ")") :: Maybe Coord
          case coord of
            Nothing -> getMove state
            Just c -> if Nothing == board Map.! c then return c else getMove state

playFromState :: GameState -> IO ()
playFromState state@(GameState player board) = do
            print state
            let result = getGameResult state in
              case result of
              Nothing -> do
                --input <- getLine
                --let coord = readMaybe ("(" ++ input ++ ")") :: Maybe Coord
                coord <- getMove state
                playFromState
                  $ GameState (otherPlayer player)
                  $ Map.insert coord (Just player) board
              Just Tie -> putStrLn "It's a tie."
              Just (Win winner) -> putStrLn $ show winner ++ " wins."

main :: IO ()
main = playFromState startState