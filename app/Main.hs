module Main where

import           Control.Monad (forM_)
import           Data.List     (intersperse)
import           System.IO
import           TicTacToe

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  gameLoop (firstMove X)

gameLoop :: Move -> IO ()
gameLoop (Move displayInfo result) = do
  putBoardLn displayInfo >> putStr ""

  case result of
    GameTied -> putStrLn "Draw."
    GameWonBy piece -> putStr (show piece) >> putStrLn " wins."
    GameInProgress moveInfos -> pickMove moveInfos >>= gameLoop

pickMove :: NextValidMoves -> IO Move
pickMove moveInfos = do
  putPiece $ getPiece (head moveInfos)
  putStr "'s, pick your move: "

  moveNumber <- readLn
  if 1 <= moveNumber || moveNumber <= length moveInfos
    then return $ getMove (moveInfos !! (moveNumber - 1))
    else putStrLn "Invalid move!" >> pickMove moveInfos

showBoard :: DisplayInfo -> String
showBoard (DisplayInfo cells) =
    unlines
  . interleave "+---+---+---+"
  . map (concat . interleave "|")
  $ board
  where
    board = chunksOf 3 (zipWith showCell [1..9] cells)

    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

    showCell i (Cell _ Empty)          = surround ' ' (show i)
    showCell _ (Cell _ (MarkedWith p)) = showPiece p

    surround x xs = x : xs ++ [x]
    interleave x xs = surround x (intersperse x xs)

putBoard :: DisplayInfo -> IO ()
putBoard = putStr . showBoard

putBoardLn :: DisplayInfo -> IO ()
putBoardLn di = putBoard di >> putStrLn ""

showPiece :: Piece -> String
showPiece X = "XXX"
showPiece O = "OOO"

putPiece :: Piece -> IO ()
putPiece = putStr . showPiece

putPieceLn :: Piece -> IO ()
putPieceLn p = putPiece p >> putStrLn ""
