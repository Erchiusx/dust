module DustUp.Artifacts.Column2.Core002
  ( definition
  ) where

import Data.List (delete, maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'Two
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "002")
    , difficulty = Text.pack "★✰✰"
    , raw'abilities =
        [ Text.pack "龙游万象：速度+1，重掷阶段可进行最多2次重掷。"
        , Text.pack "龙腾万丈：消耗3个冥想骰，获得4骰并重新分区。"
        ]
    , unsupported'rules =
        [Text.pack "速度+1和额外重掷次数尚未接入派生属性与阶段状态。"]
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'Two
buildArtifact context =
  ColumnTwo'
    { distribution =
        ( Defencing
        , Thoughtful
        , Attacking
        , Attacking
        , Thoughtful
        , Defencing
        )
    , name = Text.pack "游龙"
    , tag = Text.pack "驳纳万千锋芒的刀鞘"
    , triggers = const []
    , actived = \case
        Left'Side -> []
        Right'Side -> [soaringDragon context]
    , static = const []
    , charged = const noChargedAbility
    }

soaringDragon :: Artifact'Context -> Ability Actived
soaringDragon context =
  Active
    { validate'cost = \game costs ->
        if length costs == 3
          && all
            (\dieID -> dieIsInArea context Thoughtful dieID game)
            costs
          then Right ()
          else Left $ Text.pack "龙腾万丈必须消耗3个冥想骰。"
    , run = \game _ -> do
        faces <- roll 4
        case faces of
          [] -> pure ()
          _ -> do
            let smallest = minimumBy (comparing repr) faces
                withoutSmallest = delete smallest faces
                largest = maximumBy (comparing repr) withoutSmallest
                remaining = delete largest withoutSmallest
                attackArea = areaForCategory context Attacking
                defenceArea = areaForCategory context Defencing
            createDieInArea attackArea smallest
            createDieInArea attackArea largest
            mapM_ (createDieInArea defenceArea) remaining
        create'Modifier
          (attackDamageModifier
            (Text.pack "龙腾万丈-攻击伤害")
            context.owner'id
            1
          )
          (Just context.artifact'id)
          (Just $ endOfCurrentTurn game.time)
          Nothing
        pure ()
    }
