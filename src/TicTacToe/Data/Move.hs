module TicTacToe.Data.Move where

import qualified TicTacToe.Data.Board as B
import qualified TicTacToe.Data.Cell  as C
import qualified TicTacToe.Data.Token as T

data Move
  = Move {
    getCellPosition :: C.CellPosition
  , getToken        :: T.Token
  , getBoard        :: B.Board
  , getMoveResult   :: MoveResult
} deriving (Eq)

data MoveResult
  = GameInProgress NextValidMoves
  | GameWon
  | GameTied
  deriving (Eq)

type NextValidMoves
  = [Move]
