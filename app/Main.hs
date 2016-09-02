module Main where

import           Data.Char  (digitToInt)
import           Data.List  (intercalate, intersperse)
import           Data.Maybe (fromJust)
import           System.IO  (BufferMode (NoBuffering), hSetBuffering, stdout)
import           TicTacToe

main :: IO ()
main = gameLoop initialGameState

gameLoop :: GameState -> IO ()
gameLoop gs = do
  hSetBuffering stdout NoBuffering
  case getResult gs of
    Just result -> print result
    _ -> do
      printBoard gs
      putStr "\nMove: "
      position <- digitToInt <$> getChar
      _ <- getChar -- eat newline
      putStrLn ""

      let move = makeMove position gs

      case move of
        Left InvalidPosition -> do
          putStrLn $ "Position " ++ show position ++ " is invalid!\n"
          gameLoop gs
        Left GameIsOver -> print $ fromJust $ getResult gs
        Right gs' -> gameLoop gs'

printBoard :: GameState -> IO ()
printBoard gs =
  let board = fmap (either show show) <$> getBoard gs
  in sequence_ $ surround (putStrLn "+---+---+---+") $
     printRow <$> board
  where
    surround x xs = x : intersperse x xs ++ [x]
    printRow row = putStrLn $
      "| " ++ intercalate " | " row ++ " |"
