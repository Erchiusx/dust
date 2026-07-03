module DustUp.Artifacts.Column1.Core001
  ( definition
  ) where

import Data.Foldable (traverse_)
import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'One
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "001")
    , difficulty = Text.pack "★✰✰"
    , raw'abilities =
        [ Text.pack "中央突破：消耗最多3个冥想骰，弃置对方同等数量的防御骰。"
        , Text.pack "裁雨流：攻击骰造成伤害后，可保留点数并移动至防御或冥想区。"
        ]
    , unsupported'rules = []
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'One
buildArtifact context =
  ColumnOne'
    { speed = 4
    , will = 7
    , name = Text.pack "雨切"
    , tag = Text.pack "流浪武人的裁雨之刃"
    , triggers = \case
        Left'Side -> []
        Right'Side -> []
    , actived = \case
        Left'Side -> [centralBreakthrough context]
        Right'Side -> []
    , static = \case
        Left'Side -> []
        Right'Side -> [rainCutStyle context]
    , charged = const noChargedAbility
    }

rainCutStyle :: Artifact'Context -> Ability Static
rainCutStyle context =
  Static' $
    preserveDamagingAttackDieModifier
      (Text.pack "裁雨流")
      context.owner'id
      (areaForCategory context Defencing)

centralBreakthrough :: Artifact'Context -> Ability Actived
centralBreakthrough context =
  Active
    { validate'cost = \game costs ->
        if length costs <= 3
          && all
            (\dieID -> dieIsInArea context Thoughtful dieID game)
            costs
          then Right ()
          else Left $ Text.pack "中央突破只能消耗最多3个冥想骰。"
    , run = \game costs ->
        case opponentsOf context.owner'id game of
          opponentID : _ -> do
            if length costs == 3
              then do
                create'Modifier
                  ( unblockableAttackModifier
                      (Text.pack "中央突破-不可抵挡")
                      context.owner'id
                  )
                  (Just context.artifact'id)
                  (Just $ endOfCurrentTurn game.time)
                  Nothing
                pure ()
              else pure ()
            let
              defenceDice =
                playerAreaDice opponentID Defencing game
              discardCount = min (length costs) (length defenceDice)
            if discardCount == 0
              then pure ()
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
                case response of
                  Select _ _ selected ->
                    case playerAreaForCategory opponentID Defencing game of
                      Just areaID ->
                        traverse_ (removeDieFromArea areaID) selected
                      Nothing -> pure ()
                  _ -> pure ()
          [] -> pure ()
    }
