module Data.TicTacToe where

import           Data.Either               (isRight)
import           Data.List                 (transpose)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import           Data.Tuple                (swap)


data Cell = Cell {
    getPosition :: CellPosition
  , getState    :: CellState
} deriving (Eq, Ord, Show)

data HorizPosition =
    HLeft
  | HCenter
  | HRight
  deriving (Eq, Ord, Show)

data VertPosition =
    VTop
  | VCenter
  | VBottom
  deriving (Eq, Ord, Show)

type CellPosition = (HorizPosition, VertPosition)

validCellPositions :: [CellPosition]
validCellPositions =
  [(HLeft, VTop),    (HCenter, VTop),    (HRight, VTop)
  ,(HLeft, VCenter), (HCenter, VCenter), (HRight, VCenter)
  ,(HLeft, VBottom), (HCenter, VBottom), (HRight, VBottom)]

data CellState =
    MarkedWith Piece
  | Empty
  deriving (Eq, Ord, Show)

newtype DisplayInfo = DisplayInfo {
    getCells :: [Cell]
} deriving (Eq, Ord, Show)

-- | The Piece type. There are only two pieces: the XX and the OO.
data Piece =
    X
  | O
  deriving (Eq, Ord, Show)

data MoveInfo = MoveInfo {
    getCellPosition :: CellPosition
  , getPiece        :: Piece
  , getMove         :: Move
} deriving (Eq)

data Move = Move {
    getDisplayInfo :: DisplayInfo
  , getMoveResult  :: MoveResult
} deriving (Eq)

data MoveResult =
    GameInProgress NextValidMoves
  | GameWonBy Piece
  | GameTied
  deriving (Eq)

type NextValidMoves = [MoveInfo]

--------------------------------------------------------------------------------

firstMove :: Piece -> Move
firstMove piece = mkMove piece initialBoard

initialBoard :: Board_
initialBoard = Board_ $ (fmap Left) <$> [[1..3]
                                        ,[4..6]
                                        ,[7..9]]

newtype Board_ =
    Board_ [[Either Int Piece]]
  deriving (Eq, Show)

mkMove :: Piece -> Board_ -> Move
mkMove p b = Move (mkDisplayInfo b) (mkMoveResult p b)

mkDisplayInfo :: Board_ -> DisplayInfo
mkDisplayInfo (Board_ b) = DisplayInfo $ convert <$> zip (concat b) validCellPositions
  where convert (Left  x, pos) = Cell pos Empty
        convert (Right x, pos) = Cell pos (MarkedWith x)

mkMoveInfo :: Piece -> Board_ -> CellPosition -> MoveInfo
mkMoveInfo piece board pos = MoveInfo pos piece move
  where move = mkMove (switch piece) newBoard
        newBoard   = performMove (toInt pos) piece board
        toInt pos  = fromJust (M.lookup pos conversion)
        conversion = M.fromList (zip validCellPositions [1..9])

switch :: Piece -> Piece
switch X = O
switch O = X

performMove :: Int -> Piece -> Board_ -> Board_
performMove pos piece (Board_ b) = Board_ $ map replace <$> b
  where replace x = if x == Left pos then Right piece else x

mkMoveResult :: Piece -> Board_ -> MoveResult
mkMoveResult piece board =
  case mkResult_ board of
    Tied_       -> GameTied
    Won_        -> GameWonBy (switch piece)
    InProgress_ -> GameInProgress $ map (mkMoveInfo piece board) validCellPositions

data Result_ = Tied_ | Won_ | InProgress_

mkResult_ :: Board_ -> Result_
mkResult_ (Board_ b)
  | any winner $ rows b ++ columns b ++ diagonals b = Won_
  | all (all isRight) b = Tied_
  | otherwise = InProgress_
  where winner (x:xs) = all (== x) xs
        rows = id
        columns = transpose . rows
        diagonals [[a,_,c]
                  ,[_,d,_]
                  ,[e,_,f]] = [[a,d,f],[c,d,e]]
