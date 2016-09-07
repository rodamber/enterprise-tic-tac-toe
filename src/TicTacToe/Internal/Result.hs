module TicTacToe.Internal.Result where

import qualified TicTacToe.Data.Cell       as C
import qualified TicTacToe.Internal.Matrix as M

data Result
  = Tied
  | Won
  | InProgress
  deriving (Eq, Show)

mkResult :: M.Matrix -> Result
mkResult m
  | any M.isWinningSequence $ M.rowsOf m ++ M.columnsOf m ++ M.diagonalsOf m = Won
  | all (all C.isMarked) (M.getCells m) = Tied
  | otherwise = InProgress
