module Data.TicTacToe where

import           Data.Either               (isRight)
import           Data.List                 (transpose)
import           Data.Maybe                (isJust)
import           Data.Tuple                (swap)


data Cell = Cell {
    getPosition :: CellPosition
  , getState    :: CellState
} deriving (Eq, Ord, Show)

type CellPosition = Int

data CellState =
    MarkedWith Token
  | Empty
  deriving (Eq, Ord, Show)

newtype DisplayInfo = DisplayInfo {
    getBoard :: [[Cell]]
} deriving (Eq, Ord, Show)

-- | The Token type. There are only two tokens: the XX and the OO.
data Token =
    X
  | O
  deriving (Eq, Ord, Show)

data Move = Move {
    getCellPosition :: CellPosition
  , getToken        :: Token
  , getDisplayInfo  :: DisplayInfo
  , getMoveResult   :: MoveResult
} deriving (Eq)

data MoveResult =
    GameInProgress NextValidMoves
  | GameWon
  | GameTied
  deriving (Eq)

type NextValidMoves = [Move]

--------------------------------------------------------------------------------

initialDisplayInfo :: DisplayInfo
initialDisplayInfo = mkDisplayInfo initialBoard

initialMoves :: Token -> [Move]
initialMoves token = map (mkMove token initialBoard) [1..9]

initialBoard :: Board_
initialBoard = Board_ $ map (($ Empty) . Cell) <$> [[1..3]
                                                   ,[4..6]
                                                   ,[7..9]]

--------------------------------------------------------------------------------

newtype Board_ = Board_ {
    getCells :: [[Cell]]
} deriving (Eq, Show)

mkMove :: Token -> Board_ -> Int -> Move
mkMove token board pos =
  let newBoard = performMove pos token board
  in Move pos token (mkDisplayInfo newBoard) (mkMoveResult token newBoard)

performMove :: Int -> Token -> Board_ -> Board_
performMove pos token (Board_ b) =
  let replace x = if getPosition x == pos then Cell pos (MarkedWith token) else x
  in Board_ $ map replace <$> b

mkDisplayInfo :: Board_ -> DisplayInfo
mkDisplayInfo = DisplayInfo . getCells


mkMoveResult :: Token -> Board_ -> MoveResult
mkMoveResult token board =
  case mkResult_ board of
    Tied_       -> GameTied
    Won_        -> GameWon
    InProgress_ ->
      GameInProgress $ map (mkMove (switch token) board) (emptyPositions board)

data Result_ = Tied_ | Won_ | InProgress_

mkResult_ :: Board_ -> Result_
mkResult_ (Board_ b)
  | any winner $ rows b ++ columns b ++ diagonals b = Won_
  | all (all isMarked) b = Tied_
  | otherwise = InProgress_
  where
    winner (x:xs) = all ((&&) <$> (== markOf x) <*> isJust) (map markOf xs)
    rows = id
    columns = transpose . rows
    diagonals [[a,_,c]
              ,[_,d,_]
              ,[e,_,f]] = [[a,d,f],[c,d,e]]

isMarked :: Cell -> Bool
isMarked (Cell _ (MarkedWith _)) = True
isMarked _                       = False

markOf :: Cell -> Maybe Token
markOf (Cell _ Empty)          = Nothing
markOf (Cell _ (MarkedWith x)) = Just x

switch :: Token -> Token
switch X = O
switch O = X

emptyPositions :: Board_ -> [Int]
emptyPositions (Board_ b) = [x | (Cell x Empty) <- concat b]
