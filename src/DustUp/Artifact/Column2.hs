module Dustup.Artifact.ColumnTwo where
import DustUp.Types
import DustUp.LiteralWords

梦魇启动 :: Movement -> ActionM ()
梦魇启动 movement = do
  Player{areas=(_, _, thoughtful)} <- get'active'player -- get self
  dices <- roll 2
  put_ dices Onto (Right thoughtful) From movement
