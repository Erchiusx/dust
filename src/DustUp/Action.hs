module DustUp.Action where
{- FOURMOLU_DISABLE -}
import Control.Monad.Free (Free)
import Data.Bitmask (Bitmask8)
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

type Movement'Options = Bitmask8 Movement'Options'

-- note: use the left side of artifact III to represent
-- the state before SP, and right side to represent
-- that after SP
data ActionD movement player dice artifact side area modifier andThen
  = Deal Int Damage To player By player From movement andThen
  | Heal Int To player By player From movement andThen
  | Set'The'Counter'On artifact To Int From movement andThen
  | Turn artifact To side From movement andThen
  | Roll Int ([dice] -> andThen)
  | Put (Those dice) Onto (Either artifact area) From movement andThen
  | Remove (Those dice) From (Either artifact area) From movement andThen
  | -- monadic readers
    Who'am'I (player -> andThen)
  | Get'All'Players ([player] -> andThen)
  | -- feedback
    Request'Movement From player Movement'Options

type ActionM movement player dice artifact side area modifier
  = Free ( ActionD movement player dice artifact side area modifier )

data Transformation player dice artifact side area modifier
  = Set'The'Life'Of player To Int
  | Set'The'Counter'Of artifact To Int
  | Set'The'Activated'Side'Of artifact To side
  | Roll' dice
  | Create' modifier
  | Give'Dust'Seal To player
  | Remove'Dust'Seal From player
  | Remove' dice From (Either artifact area)
