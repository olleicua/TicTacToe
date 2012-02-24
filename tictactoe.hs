import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

data Label = A | B | C deriving (Eq, Ord, Show, Enum)
type Coord = (Label, Label)
type Board = Map Coord (Maybe Player)
data Player = X | O deriving (Eq, Ord, Show)
data GameState = GameState Player Board
data GameResult = Win Player | Tie deriving (Eq, Ord, Show)

validCoords :: [Coord]
validCoords = [(x,y) | x <- [A .. C], y <- [A .. C]]

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
getGameResult (GameState _ board) = if not $ null winningLines
                                    then Just $ Win $ head winningLines
                                    else if notElem Nothing $ Map.elems board
                                    then Just Tie else Nothing
  where winningLines = catMaybes (fmap isWinningLine
                             (fmap (fmap (board Map.!))
                                   straightLines))

main :: IO ()
main = undefined