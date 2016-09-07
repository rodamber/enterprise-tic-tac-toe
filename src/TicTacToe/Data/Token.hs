module TicTacToe.Data.Token where

-- | The Token type. There are only two tokens: the XX and the OO.
data Token
  = X
  | O
  deriving (Eq, Ord, Show)

switch :: Token -> Token
switch X = O
switch O = X
