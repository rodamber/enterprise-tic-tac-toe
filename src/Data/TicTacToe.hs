module Data.TicTacToe where

import           Data.Either (isRight)
import           Data.List   (transpose)

-- | The current game state, which may be ongoing or may be over already.
data GameState =
    GameInProcess Piece Board
    -- ^ Game in process. Has the next piece to be played and the current board.
  | GameOver Result Board
    -- ^ Game over. Has the result of the game and the last board state.
  deriving (Eq, Show)

-- | Data type representing the game result which may either be the winner
-- piece or a draw.
data Result = Winner Piece | Draw deriving (Eq, Show)

-- | The board represented as a collection of Cells.
-- The empty board looks like:
--
--            +---+---+---+
--            | 1 | 2 | 3 |
--            +---+---+---+
--            | 4 | 5 | 6 |
--            +---+---+---+
--            | 7 | 8 | 9 |
--            +---+---+---+
--
-- where each number corresponds to an empty cell.
newtype Board = Board [[Cell]] deriving (Eq, Show)

-- | A cell of the board. A cell can be either empty, in which case it is marked
-- with a integer, or marked with one of the pieces.
type Cell = Either Int Piece

-- | The Piece type. There are only two pieces: the XX and the OO.
data Piece = X | O deriving (Eq, Show)

-- | Error type for an invalid move.
data MoveError =
    InvalidPosition
  -- ^ Position is already marked, or is not between 1 and 9.
  | GameIsOver
  -- ^ The game has already ended.
  deriving (Eq, Show)

-- | Represents a move with possibility of failure. Tries to fill the given
-- position with the piece of the current player. An apropriate error will be
-- returned if the position given is too low, too high or already taken.
makeMove :: Int
     -- ^ The position to be marked
     -> GameState
     -- ^ The current game state.
     -> Either MoveError GameState
     -- ^ Either an error or the new game state with the updated board and the
     -- piece switched.
makeMove _ (GameOver _ _) = Left GameIsOver
makeMove pos gs@(GameInProcess piece (Board board))
  | pos `notElem` validPositions gs = Left InvalidPosition
  | otherwise = Right $
      if isGameOver newBoard
        then GameOver result newBoard
        else GameInProcess (switch piece) newBoard
  where
    newBoard  = Board $ map replace <$> board
    replace x = if x == Left pos then Right  piece else x
    result    = if won newBoard  then Winner piece else Draw

validPositions :: GameState -> [Int]
validPositions (GameOver _ _) = []
validPositions (GameInProcess _ (Board b)) = [x | Left x <- concat b]

-- | Switches the given piece to the opposite piece.
--
-- switch X = O
-- switch O = X
switch :: Piece -> Piece
switch X = O
switch O = X

-- | Given a board returns if the game is over or not.
isGameOver :: Board -> Bool
isGameOver board@(Board b) = won board || all (all isRight) b

-- | Given a board returns if we have a winner or not.
won :: Board -> Bool
won (Board b) = any full $ rows b ++ columns b ++ diagonals b
  where
    full (x:xs) = all (== x) xs
    rows = id
    columns = transpose . rows
    diagonals [[a,_,c]
              ,[_,d,_]
              ,[e,_,f]] = [[a,d,f],[c,d,e]]

-- | Given a game state, returns the result if the game is over or nothing if not.
getResult :: GameState -> Maybe Result
getResult (GameOver r _) = Just r
getResult _              = Nothing

-- | Acessor function that gives the current state of the board.
getBoard :: GameState -> [[Either Int Piece]]
getBoard (GameInProcess _ (Board b)) = b
getBoard (GameOver      _ (Board b)) = b

initialGameState = GameInProcess X emptyBoard
  where emptyBoard = Board $ map Left <$> [[1..3],[4..6],[7..9]]
