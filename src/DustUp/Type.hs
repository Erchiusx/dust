module DustUp.Type () where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Kind
import Data.Text (Text)
import Data.Typeable
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))

class
  (Typeable o, HasField "oid" (Object o) Game'ID) =>
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

data Side = Left'Side | Right'Side deriving (Eq, Enum, Bounded)
instance Eq a => Eq (Side -> a) where
  u == v = u Left'Side == v Left'Side && u Right'Side == v Right'Side

-- dices
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

-- static gameobject templates
data Column = Column'One | Column'Two | Column'Three
class HasField "name" (Artifact c) Text => Column' (c :: Column) where
  data Artifact (c :: Column)
data Ability'Type = Triggered | Actived | Static | Charged

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
    = ColumnOne
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

instance Column' Column'Two where
  data Artifact Column'Two
    = ColumnTwo
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

type Charge'Level = Int

instance Column' Column'Three where
  data Artifact Column'Three
    = ColumnThree
    { life :: Int
    , capability :: Int -- max charge number
    , name :: Text
    , triggers :: Charge'Level -> Side -> [Ability Triggered]
    , actived :: Charge'Level -> Side -> [Ability Actived]
    , static :: Charge'Level -> Side -> [Ability Static]
    , charged :: Charge'Level -> Side -> Ability Charged
    }
    deriving Generic
    deriving Eq via EqByField "name" Text (Artifact Column'Three)

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
    , life :: Int
    }

instance Ability' Triggered where
  newtype Ability Triggered = Trigger
    { run :: forall g. GameC g => g -> Action -> Transformation -> Action
    }

instance Ability' Actived where
  newtype Ability Actived = Active
    { run :: forall g. GameC g => g -> Action
    }

instance Ability' Static where
  newtype Ability Static = Static' Modifier

newtype Modifier = Modifier
  { applyModifier
      :: forall g. GameC g => g -> Action -> Transformation -> Transformation
  }

-- movement, action and transformation
data ActionD a = Roll Int ([Dice] -> a)
  deriving (Functor, Generic)
data Transformation

type ActionM = Free ActionD
type Action = ActionM ()

data Movement
data Movement'Options

-- Game
class GameC g

data Phase
  = UPKEEP
  | ROLL
  | REROLL
  | DUSTUP
  | MAIN
  | DISCARD
  | END
  deriving (Show, Eq, Enum, Bounded, Generic)

data Game'Time = Game'Time
  { round :: Int
  , player :: Game'ID
  , phase :: Phase
  }
  deriving Generic

data Game = Game
  { objects :: [(Game'ID, Game'Object)]
  , time :: Game'Time
  }

data Input'Prompt
  = Input'Prompt
  { player :: Game'ID
  , options :: Movement'Options
  , pending'continue :: Movement -> ActionM ()
  }

data Engine'Status
  = Status'Check
  | Status'Waiting Input'Prompt

-- gen free monads
$(makeFree ''ActionD)

instance Semigroup Action where
  (<>) = (>>)

instance Monoid (ActionM ()) where
  mempty = Pure ()
