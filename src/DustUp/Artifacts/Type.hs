module DustUp.Artifacts.Type where

import Data.Text (Text)
import DustUp.Type

data Card'Source
  = Core
  | Alternate
  deriving (Show, Eq)

data Card'ID = Card'ID
  { source :: Card'Source
  , number :: Text
  }
  deriving (Show, Eq)

data Artifact'Context = Artifact'Context
  { artifact'id :: Game'ID
  , owner'id :: Game'ID
  , areas :: (Game'ID, Game'ID, Game'ID)
  }

data Artifact'Definition c = Artifact'Definition
  { card'id :: Card'ID
  , difficulty :: Text
  , raw'abilities :: [Text]
  , unsupported'rules :: [Text]
  , build :: Artifact'Context -> Artifact c
  }

data SomeArtifact'Definition where
  SomeArtifact'Definition
    :: Column' c
    => Artifact'Definition c
    -> SomeArtifact'Definition

noChargedAbility :: Ability Charged
noChargedAbility = Charge $ const mempty

lookupGameObject :: Game'ID -> Game -> Maybe Game'Object
lookupGameObject objectID game =
  lookup objectID game.objects

lookupDie :: Game'ID -> Game -> Maybe (Object Ability'Die)
lookupDie dieID game =
  lookupGameObject dieID game >>= castObject @Ability'Die

lookupArea :: Game'ID -> Game -> Maybe (Object Area)
lookupArea areaID game =
  lookupGameObject areaID game >>= castObject @Area

areaForCategory
  :: Artifact'Context
  -> Category
  -> Game'ID
areaForCategory context category =
  case (category, context.areas) of
    (Attacking, (areaID, _, _)) -> areaID
    (Defencing, (_, areaID, _)) -> areaID
    (Thoughtful, (_, _, areaID)) -> areaID

areaDice
  :: Artifact'Context
  -> Category
  -> Game
  -> [Game'ID]
areaDice context category game =
  case lookupArea (areaForCategory context category) game of
    Just area -> area.dices
    Nothing -> []

dieIsInArea
  :: Artifact'Context
  -> Category
  -> Game'ID
  -> Game
  -> Bool
dieIsInArea context category dieID game =
  dieID `elem` areaDice context category game

opponentsOf :: Game'ID -> Game -> [Game'ID]
opponentsOf playerID game =
  [ objectID
  | (objectID, object) <- game.objects
  , objectID /= playerID
  , Just _ <- [castObject @Player object]
  ]

playerAreaIDs
  :: Game'ID
  -> Game
  -> Maybe (Game'ID, Game'ID, Game'ID)
playerAreaIDs playerID game = do
  Player _ _ _ _ areaIDs <-
    lookupGameObject playerID game >>= castObject @Player
  pure areaIDs

playerAreaForCategory
  :: Game'ID
  -> Category
  -> Game
  -> Maybe Game'ID
playerAreaForCategory playerID category game = do
  areaIDs <- playerAreaIDs playerID game
  pure $
    case (category, areaIDs) of
      (Attacking, (areaID, _, _)) -> areaID
      (Defencing, (_, areaID, _)) -> areaID
      (Thoughtful, (_, _, areaID)) -> areaID

playerAreaDice
  :: Game'ID
  -> Category
  -> Game
  -> [Game'ID]
playerAreaDice playerID category game =
  case playerAreaForCategory playerID category game >>= (`lookupArea` game) of
    Just area -> area.dices
    Nothing -> []

removeDieFromArea :: Game'ID -> Game'ID -> Action
removeDieFromArea areaID dieID = do
  commit $ Remove'Die'From'Area dieID areaID
  commit $ Delete'Die dieID

createDieInArea :: Game'ID -> Dice -> Action
createDieInArea areaID face = do
  dieID <- fresh'ID
  commit $ Create'Die dieID face
  commit $ Put'Die'In'Area dieID areaID

attackDamageModifier
  :: Text
  -> Game'ID
  -> Int
  -> Modifier
attackDamageModifier modifierName ownerID bonus =
  Modifier
    { name = modifierName
    , applyModifier = \_ _ transformation ->
        case transformation of
          Change'Life target amount reason@(Damage'Received Normal'Damage (Just source))
            | source == ownerID
            , target /= ownerID
            , amount < 0 ->
                (Change'Life target (amount - bonus) reason, True)
          _ -> (transformation, False)
    }
