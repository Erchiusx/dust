module DustUp.Action
  ( Movement (..)
  , Movement'Options
  , Movement'Options' (..)
  , deal
  , heal
  , set'the'counter'on
  , turn
  , roll
  , put
  , remove
  , get'self
  , get'all'players
  , request'movement
  , option'pass
  , option'dustup
  , option'reroll
  , option'attack
  , option'defence
  , option'activate
  , option'select
  , ActionM
  , Transformation (..)
  ) where
{- FOURMOLU_DISABLE -}
import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bitmask
import DustUp.LiteralWords
-- the arguments passed to Movement should be GameObject wraps
-- rather than pure player type, dice type, etc.
data Movement player dice artifact side ability item
  = Pass By player
  | DustUp artifact Certain side With dice By player
  | Reroll (Those dice) By player
  | Attack player With dice By player
  | Defence With dice By player
  | Activate artifact Certain ability With (Those dice) By player
  | Select (Those item) By player

data Movement'Options'
  = Option'Pass
  | Option'Dustup
  | Option'Reroll
  | Option'Attack
  | Option'Defence
  | Option'Activate
  | Option'Select
  deriving (Enum, Show, Eq, Bounded)

type Movement'Options = Bitmask8 Movement'Options'

$(makeFlagValues ''Movement'Options' [t|Movement'Options|])
-- usage:
-- p :: Movement'Options
-- p = Option'Pass `addFlag` noFlag
-- d :: Movement'Options
-- d = Option'Dustup `addFlag` noFlag
-- s = p .|. d

-- note: use the left side of artifact III to represent
-- the state before SP, and right side to represent
-- that after SP
data ActionD movement player dice artifact side area modifier info andThen
  = Deal Int Damage To player By player From movement andThen
  | Heal Int To player By player From movement andThen
  | Set'the'counter'on artifact To Int From movement andThen
  | Turn artifact To side From movement andThen
  | Roll Int ([dice] -> andThen)
  | Put (Those dice) Onto (Either artifact area) From movement andThen
  | Remove (Those dice) From (Either artifact area) From movement andThen
  | -- monadic readers
    Get'self (player -> andThen)
  | Get'all'players ([player] -> andThen)
  | -- feedback
    Request'movement info From player Movement'Options andThen
  deriving Functor

$(makeFree ''ActionD)

type ActionM movement player dice artifact side area modifier info
  = Free (ActionD movement player dice artifact side area modifier info)

data Transformation player dice artifact side area modifier
  = Set'The'Life'Of player To Int
  | Set'The'Counter'Of artifact To Int
  | Set'The'Activated'Side'Of artifact To side
  | Roll' dice
  | Create' modifier
  | Give'Dust'Seal To player
  | Remove'Dust'Seal From player
  | Remove' dice From (Either artifact area)

-- t :: forall movement player dice artifact side area modifier andThen. ActionM movement player dice side area modifier andThen
-- t = do
--   self <- get'self
--   request'movement From self
