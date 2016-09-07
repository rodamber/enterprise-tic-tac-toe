module TicTacToe.Game where

import qualified TicTacToe.Data.Board      as B
import qualified TicTacToe.Data.Move       as M
import qualified TicTacToe.Data.Token      as T
import qualified TicTacToe.Internal.Matrix as MX
import qualified TicTacToe.Internal.Move   as IM

startingBoard :: B.Board
startingBoard = B.Board $ MX.getCells MX.startingMatrix

startingMoves :: T.Token -> [M.Move]
startingMoves token = map (IM.mkMove token MX.startingMatrix) [1..9]
