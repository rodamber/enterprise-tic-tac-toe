module TicTacToe.Internal.Move where

import qualified TicTacToe.Data.Board      as B
import qualified TicTacToe.Data.Move       as M
import qualified TicTacToe.Data.Token      as T
import qualified TicTacToe.Internal.Matrix as MX
import qualified TicTacToe.Internal.Result as R

mkMove :: T.Token -> MX.Matrix -> Int -> M.Move
mkMove token board pos =
  let newMatrix = MX.performMove pos token board
  in M.Move pos token (B.Board $ MX.getCells newMatrix) (mkMoveResult token newMatrix)

mkMoveResult :: T.Token -> MX.Matrix -> M.MoveResult
mkMoveResult token board =
  case R.mkResult board of
    R.Tied       -> M.GameTied
    R.Won        -> M.GameWon
    R.InProgress ->
      M.GameInProgress $ map (mkMove (T.switch token) board) (MX.emptyPositions board)
