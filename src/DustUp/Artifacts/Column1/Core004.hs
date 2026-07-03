module DustUp.Artifacts.Column1.Core004
  ( definition
  ) where

import Data.Foldable (traverse_)
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
    , unsupported'rules = []
    , build = buildArtifact
    }

buildArtifact :: Artifact'Context -> Artifact Column'One
buildArtifact context =
  ColumnOne'
    { speed = 4
    , will = 8
    , name = Text.pack "不详"
    , tag = Text.pack "喋血屏戮的斩首双刃"
    , triggers = \case
        Left'Side -> []
        Right'Side -> [edgeDefended context]
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
            transform $
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
      , speed'bonus = \_ _ -> 0
      , will'bonus = \_ _ -> 0
      , defence'dice'modifier = \_ _ _ _ dice -> dice
      }

edgeDefended :: Artifact'Context -> Ability Triggered
edgeDefended context =
  Trigger $ \_ _ transformation ->
    case transformation of
      Attack'Defended attackerID defenderID _ _ attackAmount
        | attackerID == context.owner'id ->
            Just $ do
              game <- get'Game
              let
                attackDice = playerAreaDice context.owner'id Attacking game
                thoughtDice = playerAreaDice context.owner'id Thoughtful game
              if null attackDice || null thoughtDice
                then pure ()
                else do
                  requestID <- fresh'ID
                  choice <-
                    request'Movement $
                      Request'Option
                        requestID
                        context.owner'id
                        [Text.pack "不使用锋芒", Text.pack "使用锋芒"]
                        (Text.pack "攻击被抵挡。是否消耗1个攻击骰和1个冥想骰取消防御？")
                  case choice of
                    Choose'Option _ _ 1 ->
                      resolveEdgeDefended context defenderID attackAmount
                    _ -> pure ()
      _ -> Nothing

resolveEdgeDefended :: Artifact'Context -> Game'ID -> Int -> Action
resolveEdgeDefended context defenderID attackAmount = do
  attackCost <- selectOneDie context Attacking (Text.pack "选择要消耗的攻击骰。")
  thoughtCost <- selectOneDie context Thoughtful (Text.pack "选择要消耗的冥想骰。")
  case (attackCost, thoughtCost) of
    (Just attackDieID, Just thoughtDieID) -> do
      traverse_
        ( \(category, dieID) ->
            removeDieFromArea (areaForCategory context category) dieID
        )
        [ (Attacking, attackDieID)
        , (Thoughtful, thoughtDieID)
        ]
      deal'Damage
        attackAmount
        Normal'Damage
        (Just context.owner'id)
        defenderID
      deal'Damage
        2
        True'Damage
        (Just context.owner'id)
        defenderID
    _ -> pure ()

selectOneDie :: Artifact'Context -> Category -> Text.Text -> InterpreterM (Maybe Game'ID)
selectOneDie context category prompt = do
  game <- get'Game
  case playerAreaDice context.owner'id category game of
    [] -> pure Nothing
    dieIDs -> do
      requestID <- fresh'ID
      response <-
        request'Movement $
          Request'Select
            requestID
            context.owner'id
            dieIDs
            (Selection'Constraint 1 1)
            prompt
      pure $ case response of
        Select _ _ [dieID] -> Just dieID
        _ -> Nothing
