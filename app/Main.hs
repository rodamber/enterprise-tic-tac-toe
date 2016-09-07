module Main where

import           Control.Monad (forM_)
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
  if 1 <= moveNumber && moveNumber <= length moves
    then return $ moves !! (moveNumber - 1)
    else putStrLn "Invalid move!" >> pickMove moves

showBoard :: DisplayInfo -> String
showBoard (DisplayInfo cells) =
    unlines
  . pad
  . interleave "+---+---+---+"
  . map (concat . interleave "|")
  $ board
  where
    board = chunksOf 3 (zipWith showCell [1..9] cells)

    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

    showCell i (Cell _ Empty)          = surround ' ' (show i)
    showCell _ (Cell _ (MarkedWith p)) = showToken p

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
