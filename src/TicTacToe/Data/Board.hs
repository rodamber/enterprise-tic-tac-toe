module TicTacToe.Data.Board where

import qualified TicTacToe.Data.Cell as C

newtype Board
  = Board {
    getCells :: [[C.Cell]]
  } deriving (Eq, Ord)
