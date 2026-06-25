module DustUp.Artifacts.Column1.Core004
  ( definition
  ) where

import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'One
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "004")
    , difficulty = Text.pack "★✰✰"
    , raw'abilities =
        [ Text.pack "淬炼：消耗1个攻击骰，本神器计数+1，最高3层。"
        , Text.pack "锋芒：攻击伤害增加本神器计数；攻击被抵挡后可追加效果。"
        ]
    , unsupported'rules =
        [Text.pack "攻击被防御骰抵挡目前没有独立事件，锋芒的触发部分尚未接入。"]
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'One
buildArtifact context =
  ColumnOne'
    { speed = 4
    , will = 8
    , name = Text.pack "不详"
    , tag = Text.pack "喋血屏戮的斩首双刃"
    , triggers = const []
    , actived = \case
        Left'Side -> [tempering context]
        Right'Side -> []
    , static = \case
        Left'Side -> []
        Right'Side -> [edge context]
    , charged = const noChargedAbility
    }

tempering :: Artifact'Context -> Ability Actived
tempering context =
  Active
    { validate'cost = \game costs ->
        case costs of
          [dieID]
            | dieIsInArea context Attacking dieID game -> Right ()
          _ -> Left $ Text.pack "淬炼必须消耗1个攻击骰。"
    , run = \_ _ -> do
        game <- get'Game
        case lookupGameObject context.artifact'id game
          >>= castObject @(Artifact Column'One) of
          Just artifact ->
            commit $
              Set'Counter context.artifact'id (min 3 $ artifact.counter + 1)
          Nothing -> pure ()
    }

edge :: Artifact'Context -> Ability Static
edge context =
  Static' $
    Modifier
      { name = Text.pack "锋芒"
      , applyModifier = \game _ transformation ->
          case transformation of
            Change'Life target amount reason@(Damage'Received Normal'Damage (Just source))
              | source == context.owner'id
              , target /= context.owner'id
              , amount < 0 ->
                  let bonus =
                        case lookupGameObject context.artifact'id game
                          >>= castObject @(Artifact Column'One) of
                          Just artifact -> artifact.counter
                          Nothing -> 0
                   in (Change'Life target (amount - bonus) reason, bonus > 0)
            _ -> (transformation, False)
      }
