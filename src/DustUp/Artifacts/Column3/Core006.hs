module DustUp.Artifacts.Column3.Core006
  ( definition
  ) where

import Data.Foldable (traverse_)
import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'Three
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "006")
    , difficulty = Text.pack "★✰✰"
    , raw'abilities =
        [ Text.pack "零时枭首：充能时获得等同充能计数的攻击骰。"
        , Text.pack "必杀：消耗1个攻击骰，弃置对方X个防御骰，并可能获得X个攻击骰。"
        ]
    , unsupported'rules = []
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'Three
buildArtifact context =
  ColumnThree'
    { life = 50
    , capability = 4
    , name = Text.pack "完杀"
    , triggers = \_ _ -> []
    , actived = \_ _ -> []
    , static = \_ _ -> []
    , charged = \_ _ -> executionCharge context
    , ultimate = zeroHourExecution context
    }

executionCharge :: Artifact'Context -> Ability Charged
executionCharge context =
  Charge $ \game ->
    case lookupGameObject context.artifact'id game
      >>= castObject @(Artifact Column'Three) of
      Just artifact ->
        createDieInArea
          (areaForCategory context Attacking)
          (fromInteger $ toInteger artifact.charge)
      Nothing -> pure ()

zeroHourExecution :: Artifact'Context -> Ability Actived
zeroHourExecution context =
  Active
    { validate'cost = \game costs ->
        case costs of
          [dieID]
            | dieIsInArea context Attacking dieID game -> Right ()
          _ -> Left $ Text.pack "零时枭首必须消耗1个攻击骰。"
    , run = \game costs ->
        case costs >>= (\dieID -> maybe [] pure $ lookupDie dieID game) of
          die : _ -> resolveUltimate context (fromInteger $ repr die.face)
          [] -> pure ()
    }

resolveUltimate :: Artifact'Context -> Int -> Action
resolveUltimate context amount = do
  game <- get'Game
  case opponentsOf context.owner'id game of
    opponentID : _ -> do
      let opponentAreas = playerAreas opponentID game
          defenceArea = case opponentAreas of
            Just (_, areaID, _) -> areaID
            Nothing -> -1
          defenceDice =
            maybe [] (.dices) $ lookupArea defenceArea game
          discardCount = min amount (length defenceDice)
      selected <-
        if discardCount == 0
          then pure []
          else do
            requestID <- fresh'ID
            response <-
              request'Movement $
                Request'Select
                  requestID
                  context.owner'id
                  defenceDice
                  (Selection'Constraint discardCount discardCount)
                  (Text.pack "选择对方要弃置的防御骰。")
            pure $ case response of
              Select _ _ dieIDs -> dieIDs
              _ -> []
      traverse_ (removeDieFromArea defenceArea) selected
      if length defenceDice > discardCount
        then do
          faces <- roll amount
          traverse_
            (createDieInArea $ areaForCategory context Attacking)
            faces
        else pure ()
    [] -> pure ()

playerAreas
  :: Game'ID
  -> Game
  -> Maybe (Game'ID, Game'ID, Game'ID)
playerAreas playerID game = do
  Player _ _ _ _ areas <-
    lookupGameObject playerID game >>= castObject @Player
  pure areas
