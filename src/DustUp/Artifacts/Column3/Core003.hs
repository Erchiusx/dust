module DustUp.Artifacts.Column3.Core003
  ( definition
  ) where

import Data.Foldable (traverse_)
import Data.Text qualified as Text
import DustUp.Artifacts.Type
import DustUp.Type

definition :: Artifact'Definition Column'Three
definition =
  Artifact'Definition
    { card'id = Card'ID Core (Text.pack "003")
    , difficulty = Text.pack "★★✰"
    , raw'abilities =
        [ Text.pack "斩神切：充能后可立即再次进行一次正常支付冥想骰的尘起。"
        , Text.pack "必杀：弃置双方对应类别骰，造成总弃置数的真实伤害并获得3个防御骰。"
        ]
    , unsupported'rules =
        [Text.pack "充能后的额外尘起需要支持以 DustUp 作为技能请求。"]
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'Three
buildArtifact context =
  ColumnThree'
    { life = 50
    , capability = 4
    , name = Text.pack "明镜"
    , triggers = \_ _ -> []
    , actived = \_ _ -> []
    , static = \_ _ -> []
    , charged = \_ _ -> noChargedAbility
    , ultimate = divineCut context
    }

divineCut :: Artifact'Context -> Ability Actived
divineCut context =
  Active
    { validate'cost = \_ costs ->
        if null costs
          then Right ()
          else Left $ Text.pack "斩神切必杀不消耗启动费用。"
    , run = \game _ ->
        case opponentsOf context.owner'id game of
          opponentID : _ -> do
            let ownByCategory =
                  [ (category, areaDice context category game)
                  | category <- [Attacking, Defencing, Thoughtful]
                  ]
                ownDiscarded = sum $ map (length . snd) ownByCategory
            opponentDiscarded <-
              fmap sum $
                traverse
                  (discardMatchingOpponentDice context opponentID game)
                  ownByCategory
            traverse_
              ( \(category, dieIDs) ->
                  traverse_
                    (removeDieFromArea $ areaForCategory context category)
                    dieIDs
              )
              ownByCategory
            deal'Damage
              (ownDiscarded + opponentDiscarded)
              True'Damage
              (Just context.owner'id)
              opponentID
            faces <- roll 3
            traverse_
              (createDieInArea $ areaForCategory context Defencing)
              faces
          [] -> pure ()
    }

discardMatchingOpponentDice
  :: Artifact'Context
  -> Game'ID
  -> Game
  -> (Category, [Game'ID])
  -> ActionM Int
discardMatchingOpponentDice context opponentID game (category, ownDice) = do
  let opponentDice = playerAreaDice opponentID category game
      discardCount = min (length ownDice) (length opponentDice)
  if discardCount == 0
    then pure 0
    else do
      requestID <- fresh'ID
      response <-
        request'Movement $
          Request'Select
            requestID
            context.owner'id
            opponentDice
            (Selection'Constraint discardCount discardCount)
            (Text.pack "选择对方对应类别要弃置的能力骰。")
      case response of
        Select _ _ selected ->
          case playerAreaForCategory opponentID category game of
            Just areaID -> do
              traverse_ (removeDieFromArea areaID) selected
              pure $ length selected
            Nothing -> pure 0
        _ -> pure 0
