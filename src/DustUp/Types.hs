module DustUp.Types where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Bitmask
import Data.Text
import Data.Traversable
import DustUp.LiteralWords
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
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

type Area = [DiceO]
data instance Game'Object Area = Area {oid :: Int, area :: Area}
  deriving Generic
data Artifact
  = ColumnOne
      { speed :: Int
      , will :: Int
      , name :: Text
      , tag :: Text
      , abilities :: Side -> [Ability]
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
      , abilities :: Side -> [Ability]
      }
  | ColumnThree
      { life :: Int
      , capability :: Int
      , name :: Text
      , charge :: Ability
      , sp :: Ability
      , abilities :: Side -> [Ability]
      }

instance HasField "cap" Artifact Int where
  getField ColumnOne{} = 1
  getField ColumnTwo{} = 1
  getField ColumnThree{capability} = capability

data Ability
  = Triggered
      { runTrigger :: forall a. Game'State -> ActionD a -> Transformation -> ActionM ()
      }
  | Activated
      { movement :: ActionM ()
      , isSP :: Bool
      }
  | Static Modifier
  | DustUped
      { action
          :: Int -- counters
          -> ActionM ()
      }

-- newtype Condition
--   = Condition
--   { checkCondition
--       :: forall a
--        . Game'State
--       -> ActionD a
--       -> Transformation
--       -> Bool
--   }

newtype Modifier
  = Modifier_
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
type ModifierO = Game'Object Modifier
type TriggerO = Game'Object Ability

data instance Game'Object Player = Player
  { oid :: Game'ID
  , life :: Int
  , artifacts :: (ArtifactO, ArtifactO, ArtifactO)
  , areas :: (AreaO, AreaO, AreaO)
  , template :: Player
  }
  deriving Generic

data instance Game'Object Dice = Dice
  { oid :: Game'ID
  , dice :: Dice
  }
  deriving Generic

data instance Game'Object Artifact = Artifact
  { oid :: Game'ID
  , template :: Artifact
  , actived'side :: Maybe Side
  , counters :: Int
  , dices :: [DiceO]
  }
  deriving Generic

data instance Game'Object Modifier = Modifier
  { oid :: Game'ID
  , modifier :: Modifier
  }
  deriving Generic

data instance Game'Object Ability = Trigger
  { oid :: Game'ID
  , ability :: Ability
  }
  deriving Generic

-- the arguments passed to Movement should be GameObject wraps
-- rather than pure player type, dice type, etc.
data Movement
  = Pass By PlayerO
  | DustUp ArtifactO Certain Side With (Either DustSeal DiceO) By PlayerO
  | Reroll (Those DiceO) By PlayerO
  | Attack PlayerO With DiceO By PlayerO
  | Defence With DiceO By PlayerO
  | Activate ArtifactO Certain Ability With (Those DiceO) By PlayerO
  | Select (Those DiceO) By PlayerO
  deriving Generic

data Movement'Options'
  = Option'Pass
  | Option'Dustup
  | Option'Reroll
  | Option'Attack
  | Option'Defence
  | Option'Activate
  | Option'Select
  deriving (Enum, Show, Eq, Bounded, Generic)

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
  | Create'Modifier Modifier From Movement (ModifierO -> andThen)
  | Create'Trigger Ability From Movement (Maybe TriggerO -> andThen)
  | -- monadic readers
    Get'active'player (PlayerO -> andThen)
  | Get'all'players ([PlayerO] -> andThen)
  | -- feedback
    Request'movement String From PlayerO Movement'Options (Movement -> andThen)
  deriving (Functor, Generic)

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
  | Action'Create'Trigger
  | Action'Get'active'player
  | Action'Get'all'players
  | Action'Request'movement
  deriving (Eq, Generic)
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
typeof'action (Get'active'player _) = Action'Get'active'player
typeof'action (Get'all'players _) = Action'Get'all'players
typeof'action (Request'movement{}) = Action'Request'movement
typeof'action (Create'Trigger{}) = Action'Create'Trigger

type ActionM = Free ActionD

data Transformation
  = Modify'The'Life'Of PlayerO By Int
  | Set'The'Counter'Of ArtifactO To Int
  | Set'The'Activated'Side'Of ArtifactO To Side
  | Set' DiceO To Dice
  | Create'Modifier' ModifierO
  | Create'Trigger' TriggerO
  | Give'Dust'Seal To PlayerO
  | Remove'Dust'Seal From PlayerO
  | Put' DiceO Onto (Either ArtifactO AreaO)
  | Remove' DiceO From (Either ArtifactO AreaO)
  | Time'Advance
  deriving Generic

data Phase
  = UPKEEP
  | ROLL
  | REROLL
  | DUSTUP
  | MAIN
  | DISCARD
  | END
  deriving (Show, Eq, Enum, Bounded, Generic)

next :: Phase -> Maybe Phase
next p
  | p == maxBound = Nothing
  | otherwise = Just $ succ p

data Game'Time = Game'Time
  { round :: Int
  , player :: Game'ID
  , phase :: Phase
  }
  deriving Generic

data Game'State
  = Game'State
  { players :: [PlayerO]
  , modifiers :: [ModifierO]
  , triggers :: [TriggerO]
  , rng :: StdGen
  , game'object'count :: Int
  , dust'seal :: Maybe PlayerO
  }
  deriving Generic

init'Game
  :: Player -> Player -> StdGen -> Game'State
init'Game p1 p2 rng =
  let
    p1' =
      Player
        { oid = 0
        , life = let ColumnThree{life = l} = p1.three in l
        , artifacts =
            ( Artifact 1 p1.one Nothing 0 []
            , Artifact 2 p1.two Nothing 0 []
            , Artifact 3 p1.three Nothing 0 []
            )
        , areas = (Area 4 [], Area 5 [], Area 6 [])
        , template = p1
        }
    p2' =
      Player
        { oid = 7
        , life = let ColumnThree{life = l} = p2.three in l
        , artifacts =
            ( Artifact 8 p2.one Nothing 0 []
            , Artifact 9 p2.two Nothing 0 []
            , Artifact 10 p2.three Nothing 0 []
            )
        , areas = (Area 11 [], Area 12 [], Area 13 [])
        , template = p2
        }
   in
    Game'State
      { players =
          [ p1'
          , p2'
          ]
      , rng = rng
      , modifiers = []
      , triggers = gameRuletriggers
      , game'object'count = 14
      , dust'seal = Just p2'
      }

gameRuletriggers :: [TriggerO]
gameRuletriggers = undefined

-- gameRuletriggers =
--   [ Trigger
--       { oid = 14
--       , condition =
--           Condition
--             (\_ _ -> \case Set'The'Activated'Side'Of{} -> True; _ -> False)
--       , action =
--       }
--   ]

$(makeFree ''ActionD)
$( makeFlagValues
     ''Movement'Options'
     [t|Movement'Options|]
 )

instance Semigroup (ActionM ()) where
  (<>) = (>>)

instance Monoid (ActionM ()) where
  mempty = Pure ()
