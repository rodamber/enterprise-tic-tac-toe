module TicTacToe.Internal.Matrix where

import qualified TicTacToe.Data.Cell  as C
import qualified TicTacToe.Data.Token as T

import           Data.List  (transpose)
import           Data.Maybe (isJust)

newtype Matrix
  = Matrix { -- should be hidden
    getCells :: [[C.Cell]]
  } deriving (Eq, Show)


startingMatrix :: Matrix
startingMatrix = Matrix $ map (($ C.Empty) . C.Cell) <$> [[1..3]
                                                         ,[4..6]
                                                         ,[7..9]]

performMove :: Int -> T.Token -> Matrix -> Matrix
performMove pos token (Matrix b) =
  let replace x = if C.getPosition x == pos then C.Cell pos (C.MarkedWith token) else x
  in Matrix $ map replace <$> b

rowsOf :: Matrix -> [[C.Cell]]
rowsOf = getCells

columnsOf :: Matrix -> [[C.Cell]]
columnsOf = transpose . rowsOf

diagonalsOf :: Matrix -> [[C.Cell]]
diagonalsOf (Matrix [[a,_,c]
                    ,[_,d,_]
                    ,[e,_,f]]) = [[a,d,f],[c,d,e]]

isWinningSequence :: [C.Cell] -> Bool
isWinningSequence (x:xs) = all ((&&) <$> (== C.markOf x) <*> isJust) (map C.markOf xs)

emptyPositions :: Matrix -> [Int]
emptyPositions (Matrix b) = [x | (C.Cell x C.Empty) <- concat b]
