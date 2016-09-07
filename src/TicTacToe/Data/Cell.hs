module TicTacToe.Data.Cell where

import qualified TicTacToe.Data.Token as T

data Cell
  -- Should be hidden
  = Cell {
    getPosition :: CellPosition
  , getState    :: CellState
} deriving (Eq, Ord, Show)

type CellPosition
  = Int

data CellState
  = MarkedWith T.Token
  | Empty
  deriving (Eq, Ord, Show)

-- mkCell :: CellPosition -> CellState -> Either String Cell
-- mkCell p s
--   | 1 <= p && p <= 9 = Right Cell
--   | otherwise = Left $ "Invalid cell: " ++ show p

isEmpty :: Cell -> Bool
isEmpty (Cell _ Empty) = True
isEmpty _              = False

isMarked :: Cell -> Bool
isMarked (Cell _ (MarkedWith _)) = True
isMarked _                       = False

markOf :: Cell -> Maybe T.Token
markOf (Cell _ Empty)          = Nothing
markOf (Cell _ (MarkedWith x)) = Just x
