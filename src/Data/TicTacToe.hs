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
validCellPositions = [(h,v) | h <- [HLeft, HCenter, HRight]
                                   , v <- [VTop,  VCenter, VBottom]]

data CellState =
    MarkedWith Token
  | Empty
  deriving (Eq, Ord, Show)

newtype DisplayInfo = DisplayInfo {
    getCells :: [Cell]
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
initialMoves piece = map (mkMove piece initialBoard) [1..9]

initialBoard :: Board_
initialBoard = Board_ $ fmap Left <$> [[1..3]
                                      ,[4..6]
                                      ,[7..9]]

--------------------------------------------------------------------------------

newtype Board_ =
    Board_ [[Either Int Token]]
  deriving (Eq, Show)

mkMove :: Token -> Board_ -> Int -> Move
mkMove token board pos =
  Move { getCellPosition = intToPosition pos
       , getToken = token
       , getDisplayInfo = mkDisplayInfo newBoard
       , getMoveResult = mkMoveResult token newBoard
       }
  where
    newBoard = performMove pos token board

performMove :: Int -> Token -> Board_ -> Board_
performMove pos piece (Board_ b) = Board_ $ map replace <$> b
  where
    replace x = if x == Left pos then Right piece else x

intToPosition :: Int -> CellPosition
intToPosition pos = fromJust (M.lookup pos conversion)

conversion :: M.Map Int CellPosition
conversion = M.fromList (zip [1..9] validCellPositions)


mkDisplayInfo :: Board_ -> DisplayInfo
mkDisplayInfo (Board_ b) =
  DisplayInfo $ convert <$> zip (concat b) validCellPositions
  where
    convert (Left  _, pos) = Cell pos Empty
    convert (Right x, pos) = Cell pos (MarkedWith x)


mkMoveResult :: Token -> Board_ -> MoveResult
mkMoveResult piece board =
  case mkResult_ board of
    Tied_       -> GameTied
    Won_        -> GameWon
    InProgress_ ->
      GameInProgress $ map (mkMove (switch piece) board) (emptyPositions board)

data Result_ = Tied_ | Won_ | InProgress_

mkResult_ :: Board_ -> Result_
mkResult_ (Board_ b)
  | any winner $ rows b ++ columns b ++ diagonals b = Won_
  | all (all isRight) b = Tied_
  | otherwise = InProgress_
  where
    winner (x:xs) = all (== x) xs
    rows = id
    columns = transpose . rows
    diagonals [[a,_,c]
              ,[_,d,_]
              ,[e,_,f]] = [[a,d,f],[c,d,e]]

switch :: Token -> Token
switch X = O
switch O = X

emptyPositions :: Board_ -> [Int]
emptyPositions (Board_ b) = [x | Left x <- concat b]
