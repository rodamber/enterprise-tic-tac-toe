module TicTacToe (module Data.TicTacToe) where

import           Data.TicTacToe (GameState, MoveError (..), Piece (..),
                                 Result (..), getBoard, getResult,
                                 initialGameState, makeMove, validPositions)
