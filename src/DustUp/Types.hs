module DustUp.Types where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bitmask
import Data.Text
import DustUp.LiteralWords
import GHC.Generics (Generic)
import System.Random

type Game'ID = Int
data family Game'Object a
data Player
  = PlayerTemplate
  { one :: Artifact
  , two :: Artifact
  , three :: Artifact
  }

data Category
  = Attacking
  | Defencing
  | Thoughtful
  deriving Eq

data Dice
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Generic)

repr :: Dice -> Integer
repr = \case
  One -> 1
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6

instance Num Dice where
  fromInteger n = case abs n `mod` 6 of
    0 -> Six
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    _ -> error "unreachable"

  abs = id
  a + b = fromInteger (repr a + repr b)
  a - b = fromInteger (repr a - repr b)
  a * b = fromInteger (repr a * repr b)
  signum = const 1

type Area = [Dice]
data instance Game'Object Area = Area {oid :: Int, area :: Area}
data Artifact
  = ColumnOne
      { speed :: Int
      , will :: Int
      , name :: Text
      , tag :: Text
      , left :: Ability
      , right :: Ability
      }
  | ColumnTwo
      { distribution
          :: ( Category
             , Category
             , Category
             , Category
             , Category
             , Category
             )
      , name :: Text
      , tag :: Text
      , left :: Ability
      , right :: Ability
      }
  | ColumnThree
      { life :: Int
      , capability :: Int
      , name :: Text
      , charge :: Ability
      , sp :: Ability
      }

data Ability
  = Condition `Triggered` Movement
  | Activated Movement Bool
  | Static Modifier

newtype Condition
  = Condition
  { checkCondition
      :: forall a
       . Game'State
      -> ActionD a
      -> Transformation
      -> Bool
  }

newtype Modifier
  = Modifier
  { applyModifier
      :: forall a
       . Game'State
      -> ActionD a
      -> Transformation
      -> Transformation
  }
data Side = Left'Side | Right'Side

-- $(make'game'objects [''Player, ''Dice, ''Artifact, ''Side] ''Game'Object)
type PlayerO = Game'Object Player
type DiceO = Game'Object Dice
type ArtifactO = Game'Object Artifact
type AreaO = Game'Object Area

data instance Game'Object Player = Player
  { oid :: Int
  , life :: Int
  , artifacts :: (ArtifactO, ArtifactO, ArtifactO)
  , areas :: (AreaO, AreaO, AreaO)
  }

data instance Game'Object Dice = Dice
  { oid :: Int
  , dice :: Dice
  }

data instance Game'Object Artifact = Artifact
  { oid :: Int
  , artifact :: Artifact
  }

-- the arguments passed to Movement should be GameObject wraps
-- rather than pure player type, dice type, etc.
data Movement
  = Pass By PlayerO
  | DustUp ArtifactO Certain Side With DiceO By PlayerO
  | Reroll (Those DiceO) By PlayerO
  | Attack PlayerO With DiceO By PlayerO
  | Defence With DiceO By PlayerO
  | Activate ArtifactO Certain Ability With (Those DiceO) By PlayerO
  | Select (Those DiceO) By PlayerO

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

-- usage:
-- s = option'pass .|. option'reroll

-- note: use the left side of artifact III to represent
-- the state before SP, and right side to represent
-- that after SP
data ActionD andThen
  = Deal Int Damage To PlayerO By PlayerO From Movement andThen
  | Heal Int To PlayerO By PlayerO From Movement andThen
  | Set'the'counter'on ArtifactO To Int From Movement andThen
  | Turn ArtifactO To Side From Movement andThen
  | Roll Int ([DiceO] -> andThen)
  | Put_ (Those DiceO) Onto (Either ArtifactO AreaO) From Movement andThen
  | Flip DiceO To Dice From Movement andThen
  | Remove (Those DiceO) From (Either ArtifactO AreaO) From Movement andThen
  | Create'Modifier Modifier From Movement andThen
  | -- monadic readers
    Get'self (PlayerO -> andThen)
  | Get'all'players ([PlayerO] -> andThen)
  | -- feedback
    Request'movement String From PlayerO Movement'Options (Movement -> andThen)
  deriving Functor

-- $(make'action'types ''ActionD)
data Action'Types
  = Action'Deal
  | Action'Heal
  | Action'Set'the'counter'on
  | Action'Turn
  | Action'Roll
  | Action'Put
  | Action'Flip
  | Action'Remove
  | Action'Create'Modifier
  | Action'Get'self
  | Action'Get'all'players
  | Action'Request'movement
typeof'action :: ActionD andThen -> Action'Types
typeof'action (Deal{}) = Action'Deal
typeof'action (Heal{}) = Action'Heal
typeof'action (Set'the'counter'on{}) =
  Action'Set'the'counter'on
typeof'action (Turn{}) = Action'Turn
typeof'action (Roll _ _) = Action'Roll
typeof'action (Put_{}) = Action'Put
typeof'action (Flip{}) = Action'Flip
typeof'action (Remove{}) = Action'Remove
typeof'action (Create'Modifier{}) = Action'Create'Modifier
typeof'action (Get'self _) = Action'Get'self
typeof'action (Get'all'players _) = Action'Get'all'players
typeof'action (Request'movement{}) = Action'Request'movement

type ActionM = Free ActionD

data Transformation
  = Modify'The'Life'Of PlayerO By Int
  | Set'The'Counter'Of ArtifactO To Int
  | Set'The'Activated'Side'Of ArtifactO To Side
  | Set' DiceO To Dice
  | Roll' DiceO
  | Create' Modifier
  | Give'Dust'Seal To PlayerO
  | Remove'Dust'Seal From PlayerO
  | Put' DiceO Onto (Either ArtifactO AreaO)
  | Remove' DiceO From (Either ArtifactO AreaO)
  | Time'Advance

data Phase
  = UPKEEP
  | ROLL
  | REROLL
  | DUSTUP
  | MAIN
  | DISCARD
  | END
  deriving (Show, Eq, Enum, Bounded)

next :: Phase -> Maybe Phase
next p
  | p == maxBound = Nothing
  | otherwise = Just $ succ p

data Game'Time = Game'Time
  { round :: Int
  , player :: Game'ID
  , phase :: Phase
  }

data Game'State
  = Game'State
  { players :: [PlayerO]
  , modifiers :: [Modifier]
  , rng :: StdGen
  , game'object'count :: Int
  }

init'Game
  :: Player -> Player -> StdGen -> Game'State
init'Game p1 p2 rng =
  Game'State
    { players =
        [ Player
            { oid = 0
            , life = let ColumnThree{life = l} = p1.three in l
            , artifacts =
                ( Artifact 1 p1.one
                , Artifact 2 p1.two
                , Artifact 3 p1.three
                )
            , areas = (Area 4 [], Area 5 [], Area 6 [])
            }
        , Player
            { oid = 7
            , life = let ColumnThree{life = l} = p2.three in l
            , artifacts =
                ( Artifact 8 p2.one
                , Artifact 9 p2.two
                , Artifact 10 p2.three
                )
            , areas = (Area 11 [], Area 12 [], Area 13 [])
            }
        ]
    , rng = rng
    , modifiers = []
    , game'object'count = 14
    }

$(makeFree ''ActionD)
$( makeFlagValues
     ''Movement'Options'
     [t|Movement'Options|]
 )
