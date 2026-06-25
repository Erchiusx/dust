module DustUp.Artifacts.Column2.Core008
  ( definition
  ) where

import Data.Foldable (traverse_)
import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'Two
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "008")
    , difficulty = Text.pack "★✰✰"
    , raw'abilities =
        [ Text.pack "战车：意志+1；回合开始时若有防御骰，获得1个5点攻击骰。"
        , Text.pack "王家之拳：消耗2个冥想骰，重整防御骰并获得攻击伤害加成。"
        ]
    , unsupported'rules =
        [Text.pack "派生意志修正尚未接入属性查询管线。"]
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'Two
buildArtifact context =
  ColumnTwo'
    { distribution =
        ( Defencing
        , Attacking
        , Thoughtful
        , Thoughtful
        , Attacking
        , Defencing
        )
    , name = Text.pack "铁壁"
    , tag = Text.pack "守卫不灭王权的塔盾"
    , triggers = \case
        Left'Side -> [chariot context]
        Right'Side -> []
    , actived = \case
        Left'Side -> []
        Right'Side -> [royalFist context]
    , static = const []
    , charged = const noChargedAbility
    }

chariot :: Artifact'Context -> Ability Triggered
chariot context =
  Trigger $ \_ _ transformation ->
    case transformation of
      Set'Time time
        | time.player == context.owner'id
        , time.phase == SupplyPhase ->
            Just $ do
              game <- get'Game
              if null $ areaDice context Defencing game
                then pure ()
                else createDieInArea (areaForCategory context Attacking) Five
      _ -> Nothing

royalFist :: Artifact'Context -> Ability Actived
royalFist context =
  Active
    { validate'cost = \game costs ->
        if length costs == 2
          && all
            (\dieID -> dieIsInArea context Thoughtful dieID game)
            costs
          then Right ()
          else Left $ Text.pack "王家之拳必须消耗2个冥想骰。"
    , run = \_ _ -> do
        game <- get'Game
        let defenceDice = take 2 $ areaDice context Defencing game
            missing = 2 - length defenceDice
            defenceArea = areaForCategory context Defencing
        traverse_ (removeDieFromArea defenceArea) defenceDice
        if missing > 0
          then
            deal'Damage
              (missing * 2)
              True'Damage
              Nothing
              context.owner'id
          else pure ()
        faces <- roll 4
        traverse_ (createDieInArea defenceArea) faces
        create'Modifier
          (attackDamageModifier
            (Text.pack "王家之拳-攻击伤害")
            context.owner'id
            1
          )
          (Just context.artifact'id)
          (Just $ endOfCurrentTurn game.time)
          Nothing
        pure ()
    }
