import           Control.Monad   (forM)
import           Data.Either     (isRight)
import           Data.TicTacToe
import qualified Data.TicTacToe  as T (Result (..))
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary GameState where
  arbitrary = do
    player <- arbitrary
    board  <- arbitrary
    result <- arbitrary
    oneof $ map return [GameState player board, GameOver result]

instance Arbitrary Player where
  arbitrary = oneof $ map return [XX, OO]

instance Arbitrary T.Result where
  arbitrary = do
    player <- arbitrary
    frequency [(2, return $ T.Result player), (1, return T.Draw)]

instance Arbitrary Board where
  arbitrary = Board <$> mapM arbitraryRow [[1..3],[4..6],[7..9]]
    where
      arbitraryRow xs =
        forM xs $ \x -> do
          player <- arbitrary
          oneof $ return <$> [Left x, Right player]

prop_move :: Int -> GameState -> Bool
prop_move x gs@(GameOver _) = move x gs == Left GameIsOver
prop_move x gs
  | x < 1 = move x gs == Left PositionIsTooLow
  | x > 9 = move x gs == Left PositionIsTooHigh
  | otherwise = (move x gs == Left PositionTaken) || isRight (move x gs)

qc :: IO ()
qc = verboseCheck prop_move

-- doubleMark =
--   let board = [[Left 1,   Right XX, Left 3]
--               ,[Right OO, Left 5,   Left 6]
--               ,[Left 7,   Right XX, Right OO]]
--       gs = GameState XX board
--   in it "should not be able to mark an already marked position" $ do
--     undefined

hs :: IO ()
hs = putStrLn "\nHspec tests not implemented"

main :: IO ()
main = qc >> hs
  -- hspec $ do
  --   describe "move operation" $ do
  --     it "should not mark an already marked position" $ do
  --       xs <- forM [1..9] $ \x -> move x initialGameState
  --       mapM_ (`shouldBe` Left PositionTaken) xs
