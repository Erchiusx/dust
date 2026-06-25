module DustUp.Type where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Kind
import Data.Text (Text)
import Data.Typeable
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import System.Random (StdGen)

-- game objects
-- this class introduces the types to represent game objects;
-- the type `o` is the template for this kind of objects,
-- while the associated datatype `Object o` refer to the dynamic stored data
class
  (Typeable o
  , HasField "oid" (Object o) Game'ID
  , HasField "prototype" (Object o) o) =>
  Game'Object' (o :: Type)
  where
  category :: String
  data Object o

data Game'Object where
  Object
    :: forall (o :: Type)
     . Game'Object' o
    => Object o
    -> Game'Object

castObject :: forall o. Game'Object' o => Game'Object -> Maybe (Object o)
castObject (Object (obj :: Object o')) =
  case eqT @o' @o of
    Just Refl -> Just obj
    Nothing -> Nothing

type Game'ID = Int

data Side = Left'Side | Right'Side deriving (Show, Eq, Enum, Bounded)
instance Eq a => Eq (Side -> a) where
  u == v = u Left'Side == v Left'Side && u Right'Side == v Right'Side

-- dices
data Category
  = Attacking
  | Defencing
  | Thoughtful
  deriving (Show, Eq)

data Dice
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Show, Eq, Generic)

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

data Ability'Die = Ability'Die

instance Game'Object' Ability'Die where
  category = "ability-die"
  data Object Ability'Die = Ability'Die'Object
    { oid :: Game'ID
    , prototype :: Ability'Die
    , face :: Dice
    }

data Area = Area
  { area'category :: Category
  }

instance Game'Object' Area where
  category = "area"
  data Object Area = Area'Object
    { oid :: Game'ID
    , prototype :: Area
    , owner :: Game'ID
    , dices :: [Game'ID]
    }

-- static gameobject templates
data Column = Column'One | Column'Two | Column'Three
class HasField "name" (Artifact c) Text => Column' (c :: Column) where
  data Artifact (c :: Column)
data Ability'Type
  = Actived
  | Triggered
  | Static
  | Charged -- column 3 charge, column 1 2 activating side

class Ability' (t :: Ability'Type) where
  data Ability t :: Type

newtype EqByField (field :: Symbol) b a
  = EqByField a

instance
  ( HasField field a b
  , Eq b
  )
  => Eq (EqByField field b a)
  where
  EqByField x == EqByField y =
    getField @field x == getField @field y

instance Column' Column'One where
  data Artifact Column'One
    = ColumnOne'
    { speed :: Int
    , will :: Int
    , name :: Text
    , tag :: Text
    , triggers :: Side -> [Ability Triggered]
    , actived :: Side -> [Ability Actived]
    , static :: Side -> [Ability Static]
    , charged :: Side -> Ability Charged
    }
    deriving Generic
    deriving Eq via EqByField "name" Text (Artifact Column'One)

instance Game'Object' (Artifact Column'One) where
  category = "artifact-column-one"
  data Object (Artifact Column'One)
    = ColumnOne
    { oid :: Game'ID
    , owner :: Game'ID
    , activated :: Bool
    , actived'side :: Side
    , prototype :: Artifact Column'One
    , triggers :: Side -> [Game'ID] -- ids for triggered abilities
    , actived :: Side -> [Game'ID]
    , static :: Side -> [Game'ID]
    , charged :: Side -> Game'ID
    , counter :: Int
    -- Only interactive ability dice are stored here. Charge indicators are
    -- represented by the column-three artifact's charge field.
    , dices :: [Game'ID]
    }

instance Column' Column'Two where
  data Artifact Column'Two
    = ColumnTwo'
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
    , triggers :: Side -> [Ability Triggered]
    , actived :: Side -> [Ability Actived]
    , static :: Side -> [Ability Static]
    , charged :: Side -> Ability Charged
    }
    deriving Generic
    deriving Eq via EqByField "name" Text (Artifact Column'Two)

instance Game'Object' (Artifact Column'Two) where
  category = "artifact-column-two"
  data Object (Artifact Column'Two)
    = ColumnTwo
      { oid :: Game'ID
      , owner :: Game'ID
      , activated :: Bool
      , prototype :: Artifact Column'Two
      , actived'side :: Side
      , triggers :: Side -> [Game'ID]
      , actived :: Side -> [Game'ID]
      , static :: Side -> [Game'ID]
      , charged :: Side -> Game'ID
      , counter :: Int
      -- Only interactive ability dice are stored here.
      , dices :: [Game'ID]
      }

type Charge'Level = Int

instance Column' Column'Three where
  data Artifact Column'Three
    = ColumnThree'
    { life :: Int
    , capability :: Int -- max charge number
    , name :: Text
    , triggers :: Charge'Level -> Side -> [Ability Triggered]
    , actived :: Charge'Level -> Side -> [Ability Actived]
    , static :: Charge'Level -> Side -> [Ability Static]
    , charged :: Charge'Level -> Side -> Ability Charged
    , ultimate :: Ability Actived
    }
    deriving Generic
    deriving Eq via EqByField "name" Text (Artifact Column'Three)

instance Game'Object' (Artifact Column'Three) where
  category = "artifact-column-three"
  data Object (Artifact Column'Three)
    = ColumnThree
      { oid :: Game'ID
      , owner :: Game'ID
      , activated :: Bool
      , prototype :: Artifact Column'Three
      , actived'side :: Side
      , charge :: Charge'Level
      , triggers :: Charge'Level -> Side -> [Game'ID]
      , actived :: Charge'Level -> Side -> [Game'ID]
      , static :: Charge'Level -> Side -> [Game'ID]
      , charged :: Charge'Level -> Side -> Game'ID
      , ultimate :: Game'ID
      , ultimate'activated :: Bool
      , counter :: Int
      -- Charge is engine state, not a die object. This list contains only
      -- interactive ability dice placed on the artifact.
      , dices :: [Game'ID]
      }

data Player
  = PlayerTemplate
  { one :: Artifact Column'One
  , two :: Artifact Column'Two
  , three :: Artifact Column'Three
  }

instance Game'Object' Player where
  category = "player"
  data Object Player = Player
    { oid :: Game'ID
    , prototype :: Player
    , life :: Int
    , artifacts :: (Game'ID, Game'ID, Game'ID)
    , areas :: (Game'ID, Game'ID, Game'ID)
    }
    deriving Eq via EqByField "oid" Game'ID (Object Player)

instance Ability' Triggered where
  newtype Ability Triggered = Trigger
    { run
        :: Game
        -> Action'Record
        -> Transformation
        -> Maybe Action
    }

instance Ability' Actived where
  data Ability Actived = Active
    { validate'cost
        :: Game
        -> [Game'ID]
        -> Either Text ()
    , run
        :: Game
        -> [Game'ID]
        -> Action
    }

instance Ability' Charged where
  newtype Ability Charged = Charge
    { run :: Game -> Action
    }

data Modifier = Modifier
  { name :: Text
  , applyModifier
      :: Game
      -> Action'Record
      -> Transformation
      -> (Transformation, Bool)
  }

instance Show Modifier where
  show modifier = "Modifier " <> show modifier.name

instance Eq Modifier where
  left == right = left.name == right.name

instance Game'Object' Modifier where
  category = "modifier"
  data Object Modifier = Modifier'Object
    { oid :: Game'ID
    , prototype :: Modifier
    , source :: Maybe Game'ID
    , expires'at :: Maybe Game'Time
    , remaining'uses :: Maybe Int
    , enabled :: Bool
    }

instance Ability' Static where
  newtype Ability Static = Static' Modifier

data Ability'Prototype
  = Triggered'Ability (Ability Triggered)
  | Actived'Ability (Ability Actived)
  | Static'Ability (Ability Static)
  | Charged'Ability (Ability Charged)

instance Game'Object' Ability'Prototype where
  category = "ability"
  data Object Ability'Prototype = Ability'Object
    { oid :: Game'ID
    , prototype :: Ability'Prototype
    , activated :: Bool
    }

-- movement, action and transformation

-- References crossing the engine boundary stay untyped. The engine resolves
-- them through the object table and validates their type with castObject.
type Request'ID = Int

data Damage'Type
  = Normal'Damage
  | True'Damage
  deriving (Show, Eq, Generic)

data Life'Change'Reason
  = Damage'Received Damage'Type (Maybe Game'ID)
  | Healing'Received (Maybe Game'ID)
  | Rule'Effect
  deriving (Show, Eq, Generic)

data DustUp'Cost
  = Dust'Seal
  | Thought'Die Game'ID
  deriving (Show, Eq, Generic)

data Selection'Constraint = Selection'Constraint
  { minimum :: Int
  , maximum :: Int
  }
  deriving (Show, Eq, Generic)

data Movement
  = Pass
      { player :: Game'ID
      }
  | Reroll
      { player :: Game'ID
      , dices :: [Game'ID]
      }
  | DustUp
      { player :: Game'ID
      , artifact :: Game'ID
      , cost :: DustUp'Cost
      }
  | Attack
      { player :: Game'ID
      , playee :: Game'ID
      , dice :: Game'ID
      }
  | Defend
      { player :: Game'ID
      , attack :: Request'ID
      , defence'Die :: Maybe Game'ID
      }
  | Activate
      { player :: Game'ID
      , artifact :: Game'ID
      , ability :: Game'ID
      , costs :: [Game'ID]
      }
  | Select
      { player :: Game'ID
      , request :: Request'ID
      , targets :: [Game'ID]
      }
  | Choose'Option
      { player :: Game'ID
      , request :: Request'ID
      , option :: Int
      }
  deriving (Show, Eq, Generic)

data Movement'Options
  = Request'Select
      { request :: Request'ID
      , player :: Game'ID
      , objects :: [Game'ID]
      , constraint :: Selection'Constraint
      , prompt :: Text
      }
  | Request'Defence
      { request :: Request'ID
      , player :: Game'ID
      , attack'Dice :: Game'ID
      , defence'Dices :: [Game'ID]
      , prompt :: Text
      }
  | Request'Option
      { request :: Request'ID
      , player :: Game'ID
      , options :: [Text]
      , prompt :: Text
      }
  deriving (Show, Eq, Generic)

-- ActionD contains effects that require the interpreter: observation,
-- randomness, player input, semantic damage resolution, and state commits.
data ActionD andThen
  = Get'Game (Game -> andThen)
  | Get'Object Game'ID (Maybe Game'Object -> andThen)
  | Fresh'ID (Game'ID -> andThen)
  | Roll Int ([Dice] -> andThen)
  | Deal'Damage
      { amount :: Int
      , damage'Type :: Damage'Type
      , source :: Maybe Game'ID
      , target :: Game'ID
      , andThen :: andThen
      }
  | Heal
      { amount :: Int
      , source :: Maybe Game'ID
      , target :: Game'ID
      , andThen :: andThen
      }
  | Create'Modifier
      Modifier
      (Maybe Game'ID)
      (Maybe Game'Time)
      (Maybe Int)
      (Game'ID -> andThen)
  | Request'Movement Movement'Options (Movement -> andThen)
  | Commit Transformation andThen
  deriving (Functor, Generic)

-- A continuation-free ActionD projection suitable for history, display and
-- serialization. The interpreter records one value for each primitive action
-- it executes.
data Action'Record
  = Get'Game'Record
  | Get'Object'Record Game'ID
  | Fresh'ID'Record
  | Roll'Record Int
  | Deal'Damage'Record Int Damage'Type (Maybe Game'ID) Game'ID
  | Heal'Record Int (Maybe Game'ID) Game'ID
  | Create'Modifier'Record
      Text
      (Maybe Game'ID)
      (Maybe Game'Time)
      (Maybe Int)
  | Request'Movement'Record Movement'Options
  | Commit'Record Transformation
  deriving (Show, Eq, Generic)

withoutContinuation :: ActionD andThen -> Action'Record
withoutContinuation = \case
  Get'Game{} -> Get'Game'Record
  Get'Object objectID _ -> Get'Object'Record objectID
  Fresh'ID{} -> Fresh'ID'Record
  Roll amount _ -> Roll'Record amount
  Deal'Damage amount damageType source target _ ->
    Deal'Damage'Record amount damageType source target
  Heal amount source target _ ->
    Heal'Record amount source target
  Create'Modifier modifier source expiresAt remainingUses _ ->
    Create'Modifier'Record modifier.name source expiresAt remainingUses
  Request'Movement options _ ->
    Request'Movement'Record options
  Commit transformation _ ->
    Commit'Record transformation

-- Transformations are atomic facts. They are the only values persisted in
-- history and applied to Game by the engine.
data Transformation
  = Create'Die Game'ID Dice
  | Delete'Die Game'ID
  | Set'Die'Face Game'ID Dice
  | Put'Die'In'Area Game'ID Game'ID
  | Remove'Die'From'Area Game'ID Game'ID
  | Put'Die'On'Artifact Game'ID Game'ID
  | Remove'Die'From'Artifact Game'ID Game'ID
  | Change'Life Game'ID Int Life'Change'Reason
  | Set'Artifact'Activated Game'ID Bool
  | Set'Activated'Side Game'ID Side
  | Set'Charge Game'ID Charge'Level
  | Set'Counter Game'ID Int
  | Set'Ability'Activated Game'ID Bool
  | Set'Ultimate'Activated Game'ID Bool
  | Add'Modifier
      Game'ID
      Modifier
      (Maybe Game'ID)
      (Maybe Game'Time)
      (Maybe Int)
  | Set'Modifier'Enabled Game'ID Bool
  | Set'Modifier'Remaining'Uses Game'ID (Maybe Int)
  | Delete'Modifier Game'ID
  | Set'Dust'Seal (Maybe Game'ID)
  | Set'Dust'Fall Int
  | Set'Time Game'Time
  | Finish'Game [Game'ID]
  deriving (Show, Eq, Generic)

type ActionM = Free ActionD
type Action = ActionM ()

-- Game

data Phase
  = SupplyPhase
  | RerollPhase
  | DustUpPhase
  | MainPhase
  | EndPhase
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data Game'Time = Game'Time
  { round :: Int
  , player :: Game'ID
  , phase :: Phase
  }
  deriving (Show, Eq, Ord, Generic)

endOfCurrentPhase :: Game'Time -> Game'Time
endOfCurrentPhase time
  | time.phase == EndPhase = endOfCurrentTurn time
  | otherwise = time{phase = succ time.phase}

endOfCurrentTurn :: Game'Time -> Game'Time
endOfCurrentTurn time
  | time.player == 0 =
      Game'Time time.round 1 SupplyPhase
  | otherwise =
      Game'Time (time.round + 1) 0 SupplyPhase

nextPlayerPhase
  :: Game'ID
  -> Phase
  -> Game'Time
  -> Game'Time
nextPlayerPhase playerID phase current =
  let candidate = Game'Time current.round playerID phase
   in if candidate > current
        then candidate
        else Game'Time (current.round + 1) playerID phase

data History'Entry = History'Entry
  { movement :: Movement
  , actions :: [(Action'Record, [Transformation])]
  }
  deriving (Show, Eq, Generic)

newtype History = History [History'Entry]
  deriving (Show, Eq, Generic)

data Game = Game
  { objects :: [(Game'ID, Game'Object)]
  , time :: Game'Time
  , next'object'id :: Game'ID
  , dust'seal'holder :: Maybe Game'ID
  , dust'fall :: Int
  , winners :: Maybe [Game'ID]
  , random'generator :: StdGen
  , history :: History
  }
  deriving Generic

-- gen free monads
$(makeFree ''ActionD)

instance Semigroup Action where
  (<>) = (>>)

instance Monoid (ActionM ()) where
  mempty = Pure ()
