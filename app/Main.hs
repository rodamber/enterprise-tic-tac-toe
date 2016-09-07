module Main where

import           Control.Monad (forM_)
import           Data.Char     (intToDigit)
import           Data.List     (intersperse)
import           System.IO
import           TicTacToe

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putBoardLn initialDisplayInfo
  pickMove (initialMoves X) >>= gameLoop

gameLoop :: Move -> IO ()
gameLoop (Move _ token displayInfo result) = do
  putStrLn "" >> putBoardLn displayInfo

  case result of
    GameTied -> putStrLn "Draw."
    GameWon -> putStr (show token) >> putStrLn " wins."
    GameInProgress moves -> pickMove moves >>= gameLoop

pickMove :: [Move] -> IO Move
pickMove moves = do
  putToken $ getToken (head moves)
  putStr "'s, pick your move: "

  moveNumber <- readLn
  if moveNumber `elem` map getCellPosition moves
    then return $ head $ filter ((== moveNumber) . getCellPosition) moves
    else putStrLn "Invalid move!" >> pickMove moves

showBoard :: DisplayInfo -> String
showBoard (DisplayInfo cells) =
    unlines
  . pad
  . interleave "+---+---+---+"
  . map (concat . interleave "|")
  $ board
  where
    board = map showCell <$> cells

    showCell (Cell p Empty)          = surround ' ' (show p)
    showCell (Cell _ (MarkedWith t)) = showToken t

    surround x xs = x : xs ++ [x]
    interleave x xs = surround x (intersperse x xs)

    pad = map (padding ++)
    padding = replicate 5 ' '

putBoard :: DisplayInfo -> IO ()
putBoard = putStr . showBoard

putBoardLn :: DisplayInfo -> IO ()
putBoardLn di = putBoard di >> putStrLn ""

showToken :: Token -> String
showToken X = "XXX"
showToken O = "OOO"

putToken :: Token -> IO ()
putToken = putStr . showToken

putTokenLn :: Token -> IO ()
putTokenLn p = putToken p >> putStrLn ""
