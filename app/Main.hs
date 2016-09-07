module Main where

import qualified TicTacToe     as T

import           Control.Monad (forM_)
import           Data.Char     (intToDigit)
import           Data.List     (intersperse)
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putBoardLn T.startingBoard
  pickMove (T.startingMoves T.X) >>= gameLoop

gameLoop :: T.Move -> IO ()
gameLoop (T.Move _ token newBoard result) = do
  putStrLn "" >> putBoardLn newBoard

  case result of
    T.GameTied -> putStrLn "Draw."
    T.GameWon -> putStr (show token) >> putStrLn " wins."
    T.GameInProgress moves -> pickMove moves >>= gameLoop

pickMove :: [T.Move] -> IO T.Move
pickMove moves = do
  putToken $ T.getToken (head moves)
  putStr "'s, pick your move: "

  moveNumber <- readLn
  if moveNumber `elem` map T.getCellPosition moves
    then return $ head $ filter ((== moveNumber) . T.getCellPosition) moves
    else putStrLn "Invalid move!" >> pickMove moves

showBoard :: T.Board -> String
showBoard (T.Board cells) =
    unlines
  . pad
  . interleave "+---+---+---+"
  . map (concat . interleave "|")
  $ board
  where
    board = map showCell <$> cells

    showCell (T.Cell p T.Empty)          = surround ' ' (show p)
    showCell (T.Cell _ (T.MarkedWith t)) = showToken t

    surround x xs = x : xs ++ [x]
    interleave x xs = surround x (intersperse x xs)

    pad = map (padding ++)
    padding = replicate 5 ' '

putBoard :: T.Board -> IO ()
putBoard = putStr . showBoard

putBoardLn :: T.Board -> IO ()
putBoardLn di = putBoard di >> putStrLn ""

showToken :: T.Token -> String
showToken T.X = "XXX"
showToken T.O = "OOO"

putToken :: T.Token -> IO ()
putToken = putStr . showToken

putTokenLn :: T.Token -> IO ()
putTokenLn p = putToken p >> putStrLn ""
