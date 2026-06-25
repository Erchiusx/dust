module DustUp.Engine where

import Control.Monad.Free (Free (..))
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (find, nub, sort)
import Data.Text qualified as Text
import DustUp.Type
import System.Random (Random (randomR), StdGen)

data Transformation'Error
  = Object'Not'Found Game'ID
  | Object'Already'Exists Game'ID
  | Wrong'Object'Type Game'ID String
  | Die'Already'Contained Game'ID Game'ID
  | Die'Not'Contained Game'ID Game'ID
  | Die'Still'Contained Game'ID [Game'ID]
  | Invalid'Charge Game'ID Charge'Level
  | Invalid'Counter Game'ID Int
  | Invalid'Dust'Fall Int
  | Empty'Winners
  deriving (Show, Eq)

data Engine'Error
  = Transformation'Failed Transformation'Error
  | Invalid'Roll'Count Int
  | Invalid'Damage'Amount Int
  | Invalid'Healing'Amount Int
  | Invalid'Movement'Response String
  | Movement'Rejected String
  deriving (Show, Eq)

data Engine'Result
  = Engine'Completed Game
  | Engine'Failed Engine'Error Game
  | Engine'Awaiting
      Movement'Options
      (Movement -> Engine'Result)

data Engine'State = Engine'State
  { game :: Game
  , action'log :: [(Action'Record, [Transformation])]
  , trigger'depth :: Int
  }

data Engine'Step a
  = Step'Completed a Engine'State
  | Step'Failed Engine'Error
  | Step'Awaiting Movement'Options (Movement -> Engine'Step a)

newtype EngineM a = EngineM
  { runEngineM :: Engine'State -> Engine'Step a
  }

instance Functor EngineM where
  fmap f action = action >>= pure . f

instance Applicative EngineM where
  pure value =
    EngineM $ \state -> Step'Completed value state
  (<*>) = apEngine

instance Monad EngineM where
  EngineM action >>= continue =
    EngineM $ \state -> bindStep (action state) continue

apEngine :: EngineM (a -> b) -> EngineM a -> EngineM b
apEngine function argument = do
  f <- function
  value <- argument
  pure $ f value

bindStep :: Engine'Step a -> (a -> EngineM b) -> Engine'Step b
bindStep step continue =
  case step of
    Step'Completed value state ->
      runEngineM (continue value) state
    Step'Failed err ->
      Step'Failed err
    Step'Awaiting options resume ->
      Step'Awaiting options $ \movement ->
        bindStep (resume movement) continue

throwEngineError :: Engine'Error -> EngineM a
throwEngineError err =
  EngineM $ const $ Step'Failed err

-- The handler restarts from the state at which the protected computation
-- began, including after a suspended computation is resumed.
catchEngineError
  :: EngineM a
  -> (Engine'Error -> EngineM a)
  -> EngineM a
catchEngineError (EngineM action) handler =
  EngineM $ \initialState ->
    recover initialState (action initialState)
 where
  recover initialState = \case
    Step'Completed value state ->
      Step'Completed value state
    Step'Failed err ->
      runEngineM (handler err) initialState
    Step'Awaiting options resume ->
      Step'Awaiting options $ \movement ->
        recover initialState (resume movement)

getEngineGame :: EngineM Game
getEngineGame =
  EngineM $ \state -> Step'Completed state.game state

putEngineGame :: Game -> EngineM ()
putEngineGame game =
  EngineM $ \(Engine'State _ actionLog triggerDepth) ->
    Step'Completed () (Engine'State game actionLog triggerDepth)

recordAction
  :: Action'Record
  -> [Transformation]
  -> EngineM ()
recordAction action transformations =
  EngineM $ \(Engine'State game actionLog triggerDepth) ->
    Step'Completed
      ()
      ( Engine'State
          game
          (actionLog <> [(action, transformations)])
          triggerDepth
      )

getTriggerDepth :: EngineM Int
getTriggerDepth =
  EngineM $ \state -> Step'Completed state.trigger'depth state

modifyTriggerDepth :: (Int -> Int) -> EngineM ()
modifyTriggerDepth modify =
  EngineM $ \(Engine'State game actionLog triggerDepth) ->
    Step'Completed
      ()
      (Engine'State game actionLog (modify triggerDepth))

withTriggerResolution :: EngineM a -> EngineM a
withTriggerResolution action = do
  modifyTriggerDepth (+ 1)
  result <- action
  modifyTriggerDepth (\depth -> max 0 (depth - 1))
  pure result

runMovement
  :: (Movement -> EngineM ())
  -> Movement
  -> Game
  -> Engine'Result
runMovement interpretMovement movement initialGame =
  finishMovement movement initialGame $
    runEngineM
      (interpretMovement movement)
      (Engine'State initialGame [] 0)

runActionMovement
  :: (Movement -> Either Engine'Error Action)
  -> Movement
  -> Game
  -> Engine'Result
runActionMovement compileMovement =
  runMovement $ \movement ->
    do
      validateMovement movement
      case compileMovement movement of
        Left err -> throwEngineError err
        Right action -> interpretAction action

runGameMovement :: Movement -> Game -> Engine'Result
runGameMovement = runActionMovement compileMovement

runSystemAction :: Action -> Game -> Engine'Result
runSystemAction action initialGame =
  finishSystemAction initialGame $
    runEngineM (interpretAction action) (Engine'State initialGame [] 0)

finishSystemAction
  :: Game
  -> Engine'Step ()
  -> Engine'Result
finishSystemAction initialGame = \case
  Step'Completed () (Engine'State finalGame _ _) ->
    Engine'Completed finalGame
  Step'Failed err ->
    Engine'Failed err initialGame
  Step'Awaiting options resume ->
    Engine'Awaiting options $ \response ->
      finishSystemAction initialGame (resume response)

runInitialSupply :: [Game'ID] -> Game -> Engine'Result
runInitialSupply playerIDs =
  runSystemAction $ initialSupplyAction playerIDs

runSupplyPhase :: Game'ID -> Game -> Engine'Result
runSupplyPhase playerID =
  runSystemAction $ supplyPhaseAction playerID

compileMovement :: Movement -> Either Engine'Error Action
compileMovement movement =
  Right $
    case movement of
      Pass{} -> passAction
      Reroll{player = playerID, dices = dieIDs} ->
        rerollAction playerID dieIDs
      DustUp{artifact = artifactID, cost} ->
        dustUpAction artifactID cost
      Attack{player = playerID, playee = defenderID, dice = dieID} ->
        attackAction playerID defenderID dieID
      Activate
        { artifact = artifactID
        , ability = abilityID
        , costs = costIDs
        } ->
          activateAction artifactID abilityID costIDs
      Defend{} -> mempty
      Select{} -> mempty
      Choose'Option{} -> mempty

initialSupplyAction :: [Game'ID] -> Action
initialSupplyAction playerIDs = do
  initialDice <-
    traverse
      ( \playerID -> do
          game <- get'Game
          case playerSupplyData playerID game of
            Nothing -> pure (playerID, [])
            Just supply -> do
              dieIDs <- createDistributedDice supply.will supply
              pure (playerID, dieIDs)
      )
      playerIDs
  traverse_ initialRerollAction initialDice
  game <- get'Game
  commit $ Set'Time game.time{phase = RerollPhase}

supplyPhaseAction :: Game'ID -> Action
supplyPhaseAction playerID = do
  game <- get'Game
  case playerSupplyData playerID game of
    Nothing -> pure ()
    Just supply -> do
      let existing = playerAreaDieCount supply game
          available = max 0 (supply.will - existing)
          amount
            | game.time.round == 0 = 0
            | otherwise = min supply.speed available
      createDistributedDice amount supply
      pure ()
  current <- get'Game
  commit $ Set'Time current.time{phase = RerollPhase}

data Player'Supply = Player'Supply
  { player :: Game'ID
  , speed :: Int
  , will :: Int
  , distribution :: (Category, Category, Category, Category, Category, Category)
  , areas :: (Game'ID, Game'ID, Game'ID)
  }

playerSupplyData :: Game'ID -> Game -> Maybe Player'Supply
playerSupplyData playerID game = do
  Player _ _ _ (columnOneID, columnTwoID, _) areaIDs <-
    lookupObject playerID game >>= castObject @Player
  ColumnOne _ _ _ _ columnOne _ _ _ _ _ _ <-
    lookupObject columnOneID game
      >>= castObject @(Artifact Column'One)
  ColumnTwo _ _ _ columnTwo _ _ _ _ _ _ _ <-
    lookupObject columnTwoID game
      >>= castObject @(Artifact Column'Two)
  pure $
    Player'Supply
      playerID
      columnOne.speed
      columnOne.will
      columnTwo.distribution
      areaIDs

createDistributedDice
  :: Int
  -> Player'Supply
  -> ActionM [Game'ID]
createDistributedDice amount supply = do
  faces <- roll amount
  traverse
    ( \face -> do
        dieID <- fresh'ID
        commit $ Create'Die dieID face
        commit $
          Put'Die'In'Area
            dieID
            (areaForCategory (categoryForFace supply.distribution face) supply.areas)
        pure dieID
    )
    faces

initialRerollAction :: (Game'ID, [Game'ID]) -> Action
initialRerollAction (playerID, dieIDs) = do
  requestID <- fresh'ID
  response <-
    request'Movement $
      Request'Select
        requestID
        playerID
        dieIDs
        (Selection'Constraint 0 $ length dieIDs)
        (Text.pack "Choose any initial ability dice to reroll once.")
  case response of
    Select _ _ selected -> rerollAndRedistribute playerID selected
    _ -> pure ()

rerollAndRedistribute :: Game'ID -> [Game'ID] -> Action
rerollAndRedistribute playerID dieIDs = do
  faces <- roll $ length dieIDs
  traverse_
    ( \(dieID, face) -> do
        game <- get'Game
        case playerSupplyData playerID game of
          Nothing -> pure ()
          Just supply -> do
            case containingObjects dieID game of
              areaID : _ ->
                commit $ Remove'Die'From'Area dieID areaID
              [] -> pure ()
            commit $ Set'Die'Face dieID face
            commit $
              Put'Die'In'Area
                dieID
                (areaForCategory (categoryForFace supply.distribution face) supply.areas)
    )
    (zip dieIDs faces)

playerAreaDieCount :: Player'Supply -> Game -> Int
playerAreaDieCount supply game =
  length $ playerAreaDice supply game

playerAreaDice :: Player'Supply -> Game -> [Game'ID]
playerAreaDice supply game =
  concat
    [ dieIDs
    | areaID <- tupleToList supply.areas
    , Just (Area'Object _ _ _ dieIDs) <-
        [lookupObject areaID game >>= castObject @Area]
    ]

categoryForFace
  :: (Category, Category, Category, Category, Category, Category)
  -> Dice
  -> Category
categoryForFace (one, two, three, four, five, six) = \case
  One -> one
  Two -> two
  Three -> three
  Four -> four
  Five -> five
  Six -> six

areaForCategory
  :: Category
  -> (Game'ID, Game'ID, Game'ID)
  -> Game'ID
areaForCategory category (attackArea, defenceArea, thoughtArea) =
  case category of
    Attacking -> attackArea
    Defencing -> defenceArea
    Thoughtful -> thoughtArea

passAction :: Action
passAction = do
  game <- get'Game
  case game.time.phase of
    DustUpPhase -> do
      commit $ Set'Dust'Fall (min 10 $ game.dust'fall + 1)
      commit $ Set'Time game.time{phase = MainPhase}
    EndPhase ->
      endPhaseAction game.time.player
    phase ->
      commit $ Set'Time game.time{phase = nextPhase phase}

endPhaseAction :: Game'ID -> Action
endPhaseAction playerID = do
  game <- get'Game
  case playerSupplyData playerID game of
    Nothing -> pure ()
    Just supply -> do
      let areaDice = playerAreaDice supply game
          excess = max 0 (length areaDice - supply.will)
      if excess == 0
        then pure ()
        else do
          requestID <- fresh'ID
          response <-
            request'Movement $
              Request'Select
                requestID
                playerID
                areaDice
                (Selection'Constraint excess excess)
                (Text.pack "Choose excess ability dice to discard.")
          case response of
            Select _ _ selected ->
              traverse_
                ( \dieID -> do
                    current <- get'Game
                    consumeDie current dieID
                )
                selected
            _ -> pure ()
  current <- get'Game
  traverse_
    (\abilityID -> commit $ Set'Ability'Activated abilityID False)
    (allAbilityIDs current)
  final <- get'Game
  commit $ Set'Time (nextPlayerTime final)

rerollAction :: Game'ID -> [Game'ID] -> Action
rerollAction playerID dieIDs = do
  rerollAndRedistribute playerID dieIDs
  game <- get'Game
  commit $ Set'Time game.time{phase = DustUpPhase}

dustUpAction :: Game'ID -> DustUp'Cost -> Action
dustUpAction artifactID cost = do
  game <- get'Game
  payDustUpCost game cost
  case lookupObject artifactID game of
    Just object
      | Just artifact <- castObject @(Artifact Column'One) object -> do
          dustUpTwoSidedArtifact
            artifactID
            artifact.activated
            artifact.actived'side
          runChargedAbility $ artifact.charged (nextDustUpSide artifact)
      | Just artifact <- castObject @(Artifact Column'Two) object -> do
          dustUpTwoSidedArtifact
            artifactID
            artifact.activated
            artifact.actived'side
          runChargedAbility $ artifact.charged (nextDustUpSide artifact)
      | Just artifact <- castObject @(Artifact Column'Three) object -> do
          let nextCharge = artifact.charge + 1
              completed = nextCharge >= artifact.prototype.capability
          if completed
            then do
              commit $ Set'Charge artifactID 0
              commit $ Set'Artifact'Activated artifactID True
            else commit $ Set'Charge artifactID nextCharge
          runChargedAbility $
            artifact.charged
              (min artifact.prototype.capability nextCharge)
              artifact.actived'side
      | otherwise -> pure ()
    Nothing -> pure ()
  current <- get'Game
  commit $ Set'Time current.time{phase = MainPhase}

attackAction
  :: Game'ID
  -> Game'ID
  -> Game'ID
  -> Action
attackAction attackerID defenderID attackDieID = do
  game <- get'Game
  requestID <- fresh'ID
  let defenceDice = diceInAreaCategory defenderID Defencing game
      attackValue =
        maybe 0 (fromInteger . repr . (.face)) $
          lookupObject attackDieID game >>= castObject @Ability'Die
  response <-
    request'Movement $
      Request'Defence
        requestID
        defenderID
        attackDieID
        defenceDice
        (Text.pack "Choose a defence die, or decline to defend.")
  consumeDie game attackDieID
  case response of
    Defend _ _ Nothing ->
      deal'Damage
        attackValue
        Normal'Damage
        (Just attackerID)
        defenderID
    Defend _ _ (Just defenceDieID) ->
      consumeDie game defenceDieID
    _ -> pure ()

activateAction
  :: Game'ID
  -> Game'ID
  -> [Game'ID]
  -> Action
activateAction artifactID abilityID costIDs = do
  game <- get'Game
  traverse_ (consumeDie game) costIDs
  commit $ Set'Ability'Activated abilityID True
  case lookupObject artifactID game >>= castObject @(Artifact Column'Three) of
    Just artifact
      | artifact.ultimate == abilityID ->
          commit $ Set'Ultimate'Activated artifactID True
    _ -> pure ()
  case lookupObject abilityID game >>= castObject @Ability'Prototype of
    Just (Ability'Object _ (Actived'Ability (Active _ runAbility)) _) ->
      runAbility game costIDs
    _ -> pure ()

dustUpTwoSidedArtifact
  :: Game'ID
  -> Bool
  -> Side
  -> Action
dustUpTwoSidedArtifact artifactID activated side
  | activated =
      commit $ Set'Activated'Side artifactID (otherSide side)
  | otherwise =
      commit $ Set'Artifact'Activated artifactID True

class TwoSidedArtifact o where
  nextDustUpSide :: o -> Side

instance TwoSidedArtifact (Object (Artifact Column'One)) where
  nextDustUpSide artifact
    | artifact.activated = otherSide artifact.actived'side
    | otherwise = artifact.actived'side

instance TwoSidedArtifact (Object (Artifact Column'Two)) where
  nextDustUpSide artifact
    | artifact.activated = otherSide artifact.actived'side
    | otherwise = artifact.actived'side

runChargedAbility :: Game'ID -> Action
runChargedAbility abilityID = do
  game <- get'Game
  case lookupObject abilityID game >>= castObject @Ability'Prototype of
    Just (Ability'Object _ (Charged'Ability (Charge runAbility)) _) ->
      runAbility game
    _ -> pure ()

payDustUpCost :: Game -> DustUp'Cost -> Action
payDustUpCost game = \case
  Dust'Seal ->
    commit $ Set'Dust'Seal Nothing
  Thought'Die dieID ->
    consumeDie game dieID

consumeDie :: Game -> Game'ID -> Action
consumeDie game dieID = do
  case containingObjects dieID game of
    containerID : _ ->
      case lookupObject containerID game of
        Just object
          | Just _ <- castObject @Area object ->
              commit $ Remove'Die'From'Area dieID containerID
          | otherwise ->
              commit $ Remove'Die'From'Artifact dieID containerID
        Nothing -> pure ()
    [] -> pure ()
  commit $ Delete'Die dieID

diceInAreaCategory :: Game'ID -> Category -> Game -> [Game'ID]
diceInAreaCategory playerID wanted game =
  concat
    [ dieIDs
    | (_, object) <- game.objects
    , Just (Area'Object _ prototype owner dieIDs) <- [castObject @Area object]
    , owner == playerID
    , prototype.area'category == wanted
    ]

nextPhase :: Phase -> Phase
nextPhase phase
  | phase == maxBound = SupplyPhase
  | otherwise = succ phase

nextPlayerTime :: Game -> Game'Time
nextPlayerTime game =
  let playerIDs =
        sort
          [ objectID
          | (objectID, object) <- game.objects
          , Just _ <- [castObject @Player object]
          ]
      laterPlayers = filter (> game.time.player) playerIDs
      nextPlayer =
        case laterPlayers of
          playerID : _ -> playerID
          [] -> case playerIDs of
            playerID : _ -> playerID
            [] -> game.time.player
      nextRound =
        if null laterPlayers
          then game.time.round + 1
          else game.time.round
   in Game'Time nextRound nextPlayer SupplyPhase

otherSide :: Side -> Side
otherSide Left'Side = Right'Side
otherSide Right'Side = Left'Side

finishMovement
  :: Movement
  -> Game
  -> Engine'Step ()
  -> Engine'Result
finishMovement movement initialGame = \case
  Step'Completed () (Engine'State finalGame actionLog _) ->
    Engine'Completed $ recordHistory movement actionLog finalGame
  Step'Failed err ->
    Engine'Failed err initialGame
  Step'Awaiting options resume ->
    Engine'Awaiting options $ \response ->
      finishMovement movement initialGame (resume response)

validateMovement :: Movement -> EngineM ()
validateMovement movement = do
  game <- getEngineGame
  case validateMovementAgainst game movement of
    Left err -> throwEngineError err
    Right () -> pure ()

validateMovementAgainst
  :: Game
  -> Movement
  -> Either Engine'Error ()
validateMovementAgainst game movement =
  case movement of
    Pass playerID ->
      requireActivePlayer playerID game
    Reroll playerID dieIDs -> do
      requirePhase RerollPhase game
      requireActivePlayer playerID game
      requireDistinct "reroll dice" dieIDs
      speed <- playerSpeed playerID game
      if length dieIDs > speed
        then reject "the number of rerolled dice exceeds the player's speed"
        else traverse_ (\dieID -> requireDieInOwnedArea playerID Nothing dieID game) dieIDs
    DustUp playerID artifactID cost -> do
      requirePhase DustUpPhase game
      requireActivePlayer playerID game
      requireOwnedArtifact playerID artifactID game
      requireDustUpTargetAvailable artifactID game
      case cost of
        Dust'Seal
          | game.dust'seal'holder == Just playerID -> Right ()
          | otherwise -> reject "the player does not hold the dust seal"
        Thought'Die dieID ->
          requireDieInOwnedArea playerID (Just Thoughtful) dieID game
    Attack playerID playeeID dieID -> do
      requirePhase MainPhase game
      requireActivePlayer playerID game
      requirePlayer playeeID game
      if playerID == playeeID
        then reject "a player cannot attack themselves"
        else requireDieInOwnedArea playerID (Just Attacking) dieID game
    Activate playerID artifactID abilityID costIDs -> do
      requirePhase MainPhase game
      requireActivePlayer playerID game
      requireOwnedArtifact playerID artifactID game
      requireDistinct "activation costs" costIDs
      traverse_ (\dieID -> requireControlledDie playerID dieID game) costIDs
      requireAvailableAbility artifactID abilityID costIDs game
    Defend{} ->
      reject "defence is only valid as a response to a pending request"
    Select{} ->
      reject "selection is only valid as a response to a pending request"
    Choose'Option{} ->
      reject "option selection is only valid as a response to a pending request"

requirePhase :: Phase -> Game -> Either Engine'Error ()
requirePhase expected game
  | game.time.phase == expected = Right ()
  | otherwise =
      reject $
        "movement requires "
          <> show expected
          <> ", but the game is in "
          <> show game.time.phase

requireActivePlayer :: Game'ID -> Game -> Either Engine'Error ()
requireActivePlayer playerID game = do
  requirePlayer playerID game
  if game.time.player == playerID
    then Right ()
    else reject "movement was not submitted by the active player"

requirePlayer :: Game'ID -> Game -> Either Engine'Error ()
requirePlayer playerID game =
  () <$ liftTransformation (requireObject @Player playerID game)

requireOwnedArtifact
  :: Game'ID
  -> Game'ID
  -> Game
  -> Either Engine'Error ()
requireOwnedArtifact playerID artifactID game = do
  player <- liftTransformation $ requireObject @Player playerID game
  let Player _ _ _ artifactIDs _ = player
  ownerID <- artifactOwner artifactID game
  if ownerID == playerID && artifactID `elem` tupleToList artifactIDs
    then Right ()
    else reject "artifact is not registered to the acting player"

artifactOwner :: Game'ID -> Game -> Either Engine'Error Game'ID
artifactOwner artifactID game =
  case lookupObject artifactID game of
    Nothing ->
      transformationError $ Object'Not'Found artifactID
    Just object
      | Just (ColumnOne _ owner _ _ _ _ _ _ _ _ _) <-
          castObject @(Artifact Column'One) object ->
          Right owner
      | Just (ColumnTwo _ owner _ _ _ _ _ _ _ _ _) <-
          castObject @(Artifact Column'Two) object ->
          Right owner
      | Just (ColumnThree _ owner _ _ _ _ _ _ _ _ _ _ _ _) <-
          castObject @(Artifact Column'Three) object ->
          Right owner
      | otherwise ->
          transformationError $ Wrong'Object'Type artifactID "artifact"

playerSpeed :: Game'ID -> Game -> Either Engine'Error Int
playerSpeed playerID game = do
  player <- liftTransformation $ requireObject @Player playerID game
  let Player _ _ _ (columnOneID, _, _) _ = player
  artifact <-
    liftTransformation $
      requireObject @(Artifact Column'One) columnOneID game
  let ColumnOne _ _ _ _ prototype _ _ _ _ _ _ = artifact
  pure prototype.speed

requireDieInOwnedArea
  :: Game'ID
  -> Maybe Category
  -> Game'ID
  -> Game
  -> Either Engine'Error ()
requireDieInOwnedArea playerID expectedCategory dieID game = do
  () <$ liftTransformation (requireObject @Ability'Die dieID game)
  player <- liftTransformation $ requireObject @Player playerID game
  let Player _ _ _ _ areaIDs = player
  case
    [ area
    | (areaID, object) <- game.objects
    , areaID `elem` tupleToList areaIDs
    , Just area@(Area'Object _ _ owner dieIDs) <- [castObject @Area object]
    , owner == playerID
    , dieID `elem` dieIDs
    ]
    of
      [] -> reject "die is not in one of the acting player's areas"
      Area'Object _ prototype _ _ : _
        | maybe True (== prototype.area'category) expectedCategory -> Right ()
        | otherwise -> reject "die is in the wrong type of area"

requireControlledDie
  :: Game'ID
  -> Game'ID
  -> Game
  -> Either Engine'Error ()
requireControlledDie playerID dieID game = do
  () <$ liftTransformation (requireObject @Ability'Die dieID game)
  owners <-
    traverse
      (`artifactOwner` game)
      [ artifactID
      | (artifactID, object) <- game.objects
      , containsDie dieID object
      , case castObject @Area object of
          Nothing -> True
          Just _ -> False
      ]
  let inOwnedArea =
        any
          ( \(_, object) ->
              case castObject @Area object of
                Just (Area'Object _ _ owner dieIDs) ->
                  owner == playerID && dieID `elem` dieIDs
                Nothing -> False
          )
          game.objects
  if inOwnedArea || playerID `elem` owners
    then Right ()
    else reject "activation cost die is not controlled by the acting player"

requireDustUpTargetAvailable
  :: Game'ID
  -> Game
  -> Either Engine'Error ()
requireDustUpTargetAvailable artifactID game =
  case lookupObject artifactID game >>= castObject @(Artifact Column'Three) of
    Just (ColumnThree _ _ activated prototype _ charge _ _ _ _ _ _ _ _)
      | activated || charge >= prototype.capability ->
          reject "an activated or fully charged column-three artifact cannot dust up"
    _ -> Right ()

requireAvailableAbility
  :: Game'ID
  -> Game'ID
  -> [Game'ID]
  -> Game
  -> Either Engine'Error ()
requireAvailableAbility artifactID abilityID costIDs game = do
  (availableIDs, ultimateState) <- artifactAbilities artifactID game
  if abilityID `notElem` availableIDs
    then reject "ability is not available on the artifact's active side"
    else do
      ability <-
        liftTransformation $
          requireObject @Ability'Prototype abilityID game
      let Ability'Object _ prototype activated = ability
      if activated
        then reject "ability has already been activated this round"
        else do
          case ultimateState of
            Just (ultimateID, True)
              | abilityID == ultimateID ->
                  reject "ultimate ability has already been activated this game"
            _ -> Right ()
          case prototype of
            Actived'Ability (Active validateCost _) ->
              case validateCost game costIDs of
                Left message -> reject $ Text.unpack message
                Right () -> Right ()
            _ -> reject "selected ability is not an actived ability"

artifactAbilities
  :: Game'ID
  -> Game
  -> Either Engine'Error ([Game'ID], Maybe (Game'ID, Bool))
artifactAbilities artifactID game =
  case lookupObject artifactID game of
    Nothing ->
      transformationError $ Object'Not'Found artifactID
    Just object
      | Just (ColumnOne _ _ activated side _ _ actived _ _ _ _) <-
          castObject @(Artifact Column'One) object ->
          if activated
            then Right (actived side, Nothing)
            else reject "artifact is not activated"
      | Just (ColumnTwo _ _ activated _ side _ actived _ _ _ _) <-
          castObject @(Artifact Column'Two) object ->
          if activated
            then Right (actived side, Nothing)
            else reject "artifact is not activated"
      | Just (ColumnThree _ _ activated _ side charge _ actived _ _ ultimate used _ _) <-
          castObject @(Artifact Column'Three) object ->
          if activated
            then
              Right
                (actived charge side <> [ultimate], Just (ultimate, used))
            else reject "artifact is not activated"
      | otherwise ->
          transformationError $ Wrong'Object'Type artifactID "artifact"

requireDistinct
  :: String
  -> [Game'ID]
  -> Either Engine'Error ()
requireDistinct label objectIDs
  | length objectIDs == length (nub objectIDs) = Right ()
  | otherwise = reject $ label <> " contain duplicate objects"

tupleToList :: (a, a, a) -> [a]
tupleToList (one, two, three) = [one, two, three]

allAbilityIDs :: Game -> [Game'ID]
allAbilityIDs game =
  [ objectID
  | (objectID, object) <- game.objects
  , Just _ <- [castObject @Ability'Prototype object]
  ]

liftTransformation
  :: Either Transformation'Error a
  -> Either Engine'Error a
liftTransformation =
  either transformationError Right

transformationError
  :: Transformation'Error
  -> Either Engine'Error a
transformationError = Left . Transformation'Failed

reject :: String -> Either Engine'Error a
reject = Left . Movement'Rejected

interpretAction :: Action -> EngineM ()
interpretAction = \case
  Pure () -> pure ()
  Free action ->
    case action of
      Get'Game continue -> do
        game <- getEngineGame
        recordAction (withoutContinuation action) []
        interpretAction $ continue game
      Get'Object objectID continue -> do
        game <- getEngineGame
        recordAction (withoutContinuation action) []
        interpretAction $ continue (lookupObject objectID game)
      Fresh'ID continue -> do
        game <- getEngineGame
        let objectID = game.next'object'id
        putEngineGame $ setGameNextObjectID (objectID + 1) game
        recordAction (withoutContinuation action) []
        interpretAction $ continue objectID
      Roll amount continue
        | amount < 0 ->
            throwEngineError $ Invalid'Roll'Count amount
        | otherwise -> do
            game <- getEngineGame
            let (faces, generator) =
                  rollDice amount game.random'generator
            putEngineGame $ setGameRandomGenerator generator game
            recordAction (withoutContinuation action) []
            interpretAction $ continue faces
      Deal'Damage amount damageType source target continue
        | amount < 0 ->
            throwEngineError $ Invalid'Damage'Amount amount
        | otherwise -> do
            let transformation =
                  Change'Life
                    target
                    (-amount)
                    (Damage'Received damageType source)
            applyActionTransformation
              (withoutContinuation action)
              transformation
            interpretAction continue
      Heal amount source target continue
        | amount < 0 ->
            throwEngineError $ Invalid'Healing'Amount amount
        | otherwise -> do
            let transformation =
                  Change'Life
                    target
                    amount
                    (Healing'Received source)
            applyActionTransformation
              (withoutContinuation action)
              transformation
            interpretAction continue
      Create'Modifier modifier source expiresAt remainingUses continue -> do
        game <- getEngineGame
        let modifierID = game.next'object'id
        putEngineGame $ setGameNextObjectID (modifierID + 1) game
        applyActionTransformation
          (withoutContinuation action)
          (Add'Modifier modifierID modifier source expiresAt remainingUses)
        interpretAction $ continue modifierID
      Request'Movement options continue ->
        EngineM $ \state ->
          Step'Awaiting options $ \response ->
            case validateMovementResponse options response of
              Left err -> Step'Failed err
              Right () ->
                runEngineM
                  ( do
                      recordAction (withoutContinuation action) []
                      interpretAction $ continue response
                  )
                  state
      Commit transformation continue -> do
        applyActionTransformation
          (withoutContinuation action)
          transformation
        interpretAction continue

applyActionTransformation
  :: Action'Record
  -> Transformation
  -> EngineM ()
applyActionTransformation action transformation = do
  gameBefore <- getEngineGame
  let (modified, consumedModifiers) =
        applyActiveModifiers gameBefore action transformation
  case applyTransformation modified gameBefore of
    Left err ->
      throwEngineError $ Transformation'Failed err
    Right transformed -> do
      let (afterUses, useTransformations) =
            consumeModifierUses consumedModifiers transformed
      putEngineGame afterUses
      recordAction action (modified : useTransformations)
      let triggeredActions =
            activeTriggeredActions afterUses action modified
      when (not $ null triggeredActions) $
        withTriggerResolution $
          traverse_ interpretAction triggeredActions
      checkVictoryAtTriggerBoundary
      cleanupModifiersAtTriggerBoundary

checkVictoryAtTriggerBoundary :: EngineM ()
checkVictoryAtTriggerBoundary = do
  triggerDepth <- getTriggerDepth
  when (triggerDepth == 0) checkVictory

cleanupModifiersAtTriggerBoundary :: EngineM ()
cleanupModifiersAtTriggerBoundary = do
  triggerDepth <- getTriggerDepth
  when (triggerDepth == 0) cleanupExpiredModifiers

cleanupExpiredModifiers :: EngineM ()
cleanupExpiredModifiers = do
  game <- getEngineGame
  let expiredIDs =
        [ modifierID
        | (modifierID, object) <- game.objects
        , Just modifier <- [castObject @Modifier object]
        , not $ modifierIsActive game modifier
        ]
  let cleaned =
        foldl
          (\current modifierID ->
            case applyTransformation (Delete'Modifier modifierID) current of
              Right next -> next
              Left _ -> current
          )
          game
          expiredIDs
  putEngineGame cleaned
  traverse_
    (\modifierID ->
      recordAction
        (Commit'Record $ Delete'Modifier modifierID)
        [Delete'Modifier modifierID]
    )
    expiredIDs

checkVictory :: EngineM ()
checkVictory = do
  game <- getEngineGame
  case game.winners of
    Just _ -> pure ()
    Nothing ->
      case determineWinners game of
        [] -> pure ()
        winnerIDs ->
          applyActionTransformation
            (Commit'Record $ Finish'Game winnerIDs)
            (Finish'Game winnerIDs)

determineWinners :: Game -> [Game'ID]
determineWinners game
  | any ((<= 0) . snd) players =
      highestLifePlayers game.time.player players
  | game.dust'fall >= 10 =
      highestLifePlayers game.time.player players
  | otherwise = []
 where
  players =
    [ (playerID, life)
    | (playerID, object) <- game.objects
    , Just (Player _ _ life _ _) <- [castObject @Player object]
    ]

highestLifePlayers
  :: Game'ID
  -> [(Game'ID, Int)]
  -> [Game'ID]
highestLifePlayers _ [] = []
highestLifePlayers activePlayer players =
  let highestLife = foldr1 max $ map snd players
      tied =
        [ playerID
        | (playerID, life) <- players
        , life == highestLife
        ]
   in if activePlayer `elem` tied
        then [activePlayer]
        else tied

data Active'Modifier = Active'Modifier
  { object'id :: Maybe Game'ID
  , apply
      :: Game
      -> Action'Record
      -> Transformation
      -> (Transformation, Bool)
  }

activeModifiers :: Game -> [Active'Modifier]
activeModifiers game =
  staticModifiers <> temporaryModifiers
 where
  staticModifiers =
    [ Active'Modifier Nothing (applyModifier modifier)
    | abilityID <- activeStaticAbilityIDs game
    , Just (Ability'Object _ (Static'Ability (Static' modifier)) _) <-
        [lookupObject abilityID game >>= castObject @Ability'Prototype]
    ]
  temporaryModifiers =
    [ Active'Modifier (Just modifierID) (applyModifier modifier.prototype)
    | (modifierID, object) <- game.objects
    , Just modifier <- [castObject @Modifier object]
    , modifierIsActive game modifier
    ]

applyActiveModifiers
  :: Game
  -> Action'Record
  -> Transformation
  -> (Transformation, [Game'ID])
applyActiveModifiers game action transformation =
  foldl applyOne (transformation, []) $ activeModifiers game
 where
  applyOne (current, consumedIDs) modifier =
    let (modified, consumed) = modifier.apply game action current
        nextConsumed =
          case (modifier.object'id, consumed) of
            (Just modifierID, True) -> modifierID : consumedIDs
            _ -> consumedIDs
     in (modified, nextConsumed)

consumeModifierUses
  :: [Game'ID]
  -> Game
  -> (Game, [Transformation])
consumeModifierUses modifierIDs initialGame =
  foldl consumeOne (initialGame, []) modifierIDs
 where
  consumeOne (game, transformations) modifierID =
    case lookupObject modifierID game >>= castObject @Modifier of
      Just modifier ->
        case modifier.remaining'uses of
          Just remaining ->
            let transformation =
                  Set'Modifier'Remaining'Uses
                    modifierID
                    (Just $ max 0 (remaining - 1))
             in case applyTransformation transformation game of
                  Right next -> (next, transformations <> [transformation])
                  Left _ -> (game, transformations)
          Nothing -> (game, transformations)
      Nothing -> (game, transformations)

modifierIsActive :: Game -> Object Modifier -> Bool
modifierIsActive game modifier =
  modifier.enabled
    && maybe True (game.time <) modifier.expires'at
    && maybe True (> 0) modifier.remaining'uses

activeTriggeredActions
  :: Game
  -> Action'Record
  -> Transformation
  -> [Action]
activeTriggeredActions game action transformation =
  [ triggeredAction
  | abilityID <- activeTriggeredAbilityIDs game
  , Just (Ability'Object _ (Triggered'Ability (Trigger trigger)) _) <-
      [lookupObject abilityID game >>= castObject @Ability'Prototype]
  , Just triggeredAction <- [trigger game action transformation]
  ]

activeStaticAbilityIDs :: Game -> [Game'ID]
activeStaticAbilityIDs =
  activeAbilityIDs $ \case
    ActiveColumnOne _ _ static _ -> static
    ActiveColumnTwo _ _ static _ -> static
    ActiveColumnThree _ _ _ static _ -> static

activeTriggeredAbilityIDs :: Game -> [Game'ID]
activeTriggeredAbilityIDs =
  activeAbilityIDs $ \case
    ActiveColumnOne _ triggers _ _ -> triggers
    ActiveColumnTwo _ triggers _ _ -> triggers
    ActiveColumnThree _ _ triggers _ _ -> triggers

data Active'Artifact'Abilities
  = ActiveColumnOne
      Side
      [Game'ID]
      [Game'ID]
      Game'ID
  | ActiveColumnTwo
      Side
      [Game'ID]
      [Game'ID]
      Game'ID
  | ActiveColumnThree
      Charge'Level
      Side
      [Game'ID]
      [Game'ID]
      Game'ID

activeAbilityIDs
  :: (Active'Artifact'Abilities -> [Game'ID])
  -> Game
  -> [Game'ID]
activeAbilityIDs select game =
  concatMap select $
    [ abilities
    | (_, object) <- game.objects
    , Just abilities <- [activeArtifactAbilities object]
    ]

activeArtifactAbilities
  :: Game'Object
  -> Maybe Active'Artifact'Abilities
activeArtifactAbilities object
  | Just (ColumnOne _ _ True side _ triggers _ static charged _ _) <-
      castObject @(Artifact Column'One) object =
      Just $
        ActiveColumnOne
          side
          (triggers side)
          (static side)
          (charged side)
  | Just (ColumnTwo _ _ True _ side triggers _ static charged _ _) <-
      castObject @(Artifact Column'Two) object =
      Just $
        ActiveColumnTwo
          side
          (triggers side)
          (static side)
          (charged side)
  | Just (ColumnThree _ _ True _ side charge triggers _ static charged _ _ _ _) <-
      castObject @(Artifact Column'Three) object =
      Just $
        ActiveColumnThree
          charge
          side
          (triggers charge side)
          (static charge side)
          (charged charge side)
  | otherwise = Nothing

validateMovementResponse
  :: Movement'Options
  -> Movement
  -> Either Engine'Error ()
validateMovementResponse options response =
  case (options, response) of
    (Request'Select requestID playerID allowed constraint _, Select responder responseID targets)
      | responder /= playerID ->
          invalidResponse "selection was submitted by the wrong player"
      | responseID /= requestID ->
          invalidResponse "selection does not match the pending request"
      | length targets < constraint.minimum ->
          invalidResponse "too few objects were selected"
      | length targets > constraint.maximum ->
          invalidResponse "too many objects were selected"
      | length targets /= length (nub targets) ->
          invalidResponse "the same object was selected more than once"
      | any (`notElem` allowed) targets ->
          invalidResponse "selection contains an object that was not offered"
      | otherwise -> Right ()
    (Request'Defence requestID playerID _ allowed _, Defend responder responseID defence)
      | responder /= playerID ->
          invalidResponse "defence was submitted by the wrong player"
      | responseID /= requestID ->
          invalidResponse "defence does not match the pending request"
      | maybe False (`notElem` allowed) defence ->
          invalidResponse "the defence die was not offered"
      | otherwise -> Right ()
    (Request'Option requestID playerID options _, Choose'Option responder responseID option)
      | responder /= playerID ->
          invalidResponse "option was submitted by the wrong player"
      | responseID /= requestID ->
          invalidResponse "option does not match the pending request"
      | option < 0 || option >= length options ->
          invalidResponse "option index is out of range"
      | otherwise -> Right ()
    _ ->
      invalidResponse "movement is not a valid response to the pending request"
 where
  invalidResponse = Left . Invalid'Movement'Response

rollDice :: Int -> StdGen -> ([Dice], StdGen)
rollDice amount generator
  | amount <= 0 = ([], generator)
  | otherwise =
      let (face, nextGenerator) = randomR (1 :: Int, 6) generator
          (rest, finalGenerator) = rollDice (amount - 1) nextGenerator
       in (fromInteger (toInteger face) : rest, finalGenerator)

applyTransformation
  :: Transformation
  -> Game
  -> Either Transformation'Error Game
applyTransformation transformation game =
  case transformation of
    Create'Die dieID face -> do
      ensureObjectMissing dieID game
      pure
        $ setGameObjects
          ( ( dieID
            , Object (Ability'Die'Object dieID Ability'Die face)
            )
              : game.objects
          )
          game
    Delete'Die dieID -> do
      requireObject @Ability'Die dieID game
      let containers = containingObjects dieID game
      if null containers
        then
          pure $
            setGameObjects (filter ((/= dieID) . fst) game.objects) game
        else Left $ Die'Still'Contained dieID containers
    Set'Die'Face dieID face ->
      modifyObject @Ability'Die dieID
        ( \(Ability'Die'Object storedID prototype _) ->
            Right $ Ability'Die'Object storedID prototype face
        )
        game
    Put'Die'In'Area dieID areaID -> do
      requireUncontainedDie dieID game
      modifyObject @Area areaID
        ( \(Area'Object storedID prototype owner dieIDs) ->
            Right $ Area'Object storedID prototype owner (dieID : dieIDs)
        )
        game
    Remove'Die'From'Area dieID areaID -> do
      requireObject @Ability'Die dieID game
      modifyObject @Area areaID
        ( \(Area'Object storedID prototype owner dieIDs) -> do
            remaining <- removeDieReference dieID areaID dieIDs
            Right $ Area'Object storedID prototype owner remaining
        )
        game
    Put'Die'On'Artifact dieID artifactID -> do
      requireUncontainedDie dieID game
      modifyArtifact artifactID (Add'Artifact'Die dieID) game
    Remove'Die'From'Artifact dieID artifactID -> do
      requireObject @Ability'Die dieID game
      modifyArtifact artifactID (Remove'Artifact'Die dieID) game
    Change'Life playerID amount _ ->
      modifyObject @Player playerID
        ( \(Player storedID prototype currentLife artifacts areas) ->
            Right $
              Player
                storedID
                prototype
                (max 0 (currentLife + amount))
                artifacts
                areas
        )
        game
    Set'Artifact'Activated artifactID value ->
      modifyArtifact artifactID (Set'Artifact'Activation value) game
    Set'Activated'Side artifactID side ->
      modifyArtifact artifactID (Set'Artifact'Side side) game
    Set'Charge artifactID level ->
      modifyObject @(Artifact Column'Three) artifactID
        ( \(ColumnThree oid owner activated prototype side _ ts as ss cs ult used count ds) ->
            if level >= 0 && level <= prototype.capability
              then
                Right $
                  ColumnThree oid owner activated prototype side level ts as ss cs ult used count ds
              else Left $ Invalid'Charge artifactID level
        )
        game
    Set'Counter artifactID value
      | value < 0 -> Left $ Invalid'Counter artifactID value
      | otherwise ->
          modifyArtifact artifactID (Set'Artifact'Counter value) game
    Set'Ability'Activated abilityID value ->
      modifyObject @Ability'Prototype abilityID
        ( \(Ability'Object storedID prototype _) ->
            Right $ Ability'Object storedID prototype value
        )
        game
    Set'Ultimate'Activated artifactID value ->
      modifyObject @(Artifact Column'Three) artifactID
        ( \(ColumnThree oid owner activated proto side charge ts as ss cs ult _ count ds) ->
            Right $
              ColumnThree oid owner activated proto side charge ts as ss cs ult value count ds
        )
        game
    Add'Modifier modifierID modifier source expiresAt remainingUses -> do
      ensureObjectMissing modifierID game
      traverse_ (\sourceID -> requireObjectExists sourceID game) source
      pure $
        setGameObjects
          ( ( modifierID
            , Object $
                Modifier'Object
                  modifierID
                  modifier
                  source
                  expiresAt
                  remainingUses
                  True
            )
              : game.objects
          )
          game
    Set'Modifier'Enabled modifierID enabled ->
      modifyObject @Modifier modifierID
        ( \(Modifier'Object oid prototype source expiresAt uses _) ->
            Right $
              Modifier'Object
                oid
                prototype
                source
                expiresAt
                uses
                enabled
        )
        game
    Set'Modifier'Remaining'Uses modifierID remainingUses ->
      modifyObject @Modifier modifierID
        ( \(Modifier'Object oid prototype source expiresAt _ enabled) ->
            Right $
              Modifier'Object
                oid
                prototype
                source
                expiresAt
                remainingUses
                enabled
        )
        game
    Delete'Modifier modifierID -> do
      requireObject @Modifier modifierID game
      pure $
        setGameObjects
          (filter ((/= modifierID) . fst) game.objects)
          game
    Set'Dust'Seal holder -> do
      traverse_ (\playerID -> requireObject @Player playerID game) holder
      pure $ setGameDustSeal holder game
    Set'Dust'Fall level
      | level < 0 || level > 10 -> Left $ Invalid'Dust'Fall level
      | otherwise -> pure $ setGameDustFall level game
    Set'Time newTime -> do
      requireObject @Player newTime.player game
      pure $ setGameTime newTime game
    Finish'Game winnerIDs
      | null winnerIDs -> Left Empty'Winners
      | otherwise -> do
          traverse_ (\playerID -> requireObject @Player playerID game) winnerIDs
          pure $ setGameWinners (Just winnerIDs) game

recordHistory
  :: Movement
  -> [(Action'Record, [Transformation])]
  -> Game
  -> Game
recordHistory movement actions
  (Game objects time nextID seal fall winners rng (History entries)) =
    Game
      objects
      time
      nextID
      seal
      fall
      winners
      rng
      (History $ entries <> [History'Entry movement actions])

lookupObject :: Game'ID -> Game -> Maybe Game'Object
lookupObject objectID game = snd <$> find ((== objectID) . fst) game.objects

requireObject
  :: forall o
   . Game'Object' o
  => Game'ID
  -> Game
  -> Either Transformation'Error (Object o)
requireObject objectID game = do
  object <- maybe (Left $ Object'Not'Found objectID) Right $
    lookupObject objectID game
  maybe
    (Left $ Wrong'Object'Type objectID (category @o))
    Right
    (castObject @o object)

modifyObject
  :: forall o
   . Game'Object' o
  => Game'ID
  -> (Object o -> Either Transformation'Error (Object o))
  -> Game
  -> Either Transformation'Error Game
modifyObject objectID modify game = do
  object <- requireObject @o objectID game
  modified <- modify object
  pure $ replaceObject objectID (Object modified) game

data Artifact'Change
  = Set'Artifact'Activation Bool
  | Set'Artifact'Side Side
  | Set'Artifact'Counter Int
  | Add'Artifact'Die Game'ID
  | Remove'Artifact'Die Game'ID

modifyArtifact
  :: Game'ID
  -> Artifact'Change
  -> Game
  -> Either Transformation'Error Game
modifyArtifact artifactID change game =
  case lookupObject artifactID game of
    Nothing -> Left $ Object'Not'Found artifactID
    Just object
      | Just artifact <- castObject @(Artifact Column'One) object -> do
          modified <- modifyColumnOne change artifact
          Right $ replaceObject artifactID (Object modified) game
      | Just artifact <- castObject @(Artifact Column'Two) object -> do
          modified <- modifyColumnTwo change artifact
          Right $ replaceObject artifactID (Object modified) game
      | Just artifact <- castObject @(Artifact Column'Three) object -> do
          modified <- modifyColumnThree change artifact
          Right $ replaceObject artifactID (Object modified) game
      | otherwise ->
          Left $ Wrong'Object'Type artifactID "artifact"

modifyColumnOne
  :: Artifact'Change
  -> Object (Artifact Column'One)
  -> Either Transformation'Error (Object (Artifact Column'One))
modifyColumnOne change artifact =
  case (change, artifact) of
    (Set'Artifact'Activation activated, ColumnOne oid owner _ side proto ts as ss cs count ds) ->
      Right $ ColumnOne oid owner activated side proto ts as ss cs count ds
    (Set'Artifact'Side side, ColumnOne oid owner activated _ proto ts as ss cs count ds) ->
      Right $ ColumnOne oid owner activated side proto ts as ss cs count ds
    (Set'Artifact'Counter count, ColumnOne oid owner activated side proto ts as ss cs _ ds) ->
      Right $ ColumnOne oid owner activated side proto ts as ss cs count ds
    (Add'Artifact'Die dieID, ColumnOne oid owner activated side proto ts as ss cs count ds) ->
      Right $ ColumnOne oid owner activated side proto ts as ss cs count (dieID : ds)
    (Remove'Artifact'Die dieID, ColumnOne oid owner activated side proto ts as ss cs count ds) -> do
      remaining <- removeDieReference dieID oid ds
      Right $ ColumnOne oid owner activated side proto ts as ss cs count remaining

modifyColumnTwo
  :: Artifact'Change
  -> Object (Artifact Column'Two)
  -> Either Transformation'Error (Object (Artifact Column'Two))
modifyColumnTwo change artifact =
  case (change, artifact) of
    (Set'Artifact'Activation activated, ColumnTwo oid owner _ proto side ts as ss cs count ds) ->
      Right $ ColumnTwo oid owner activated proto side ts as ss cs count ds
    (Set'Artifact'Side side, ColumnTwo oid owner activated proto _ ts as ss cs count ds) ->
      Right $ ColumnTwo oid owner activated proto side ts as ss cs count ds
    (Set'Artifact'Counter count, ColumnTwo oid owner activated proto side ts as ss cs _ ds) ->
      Right $ ColumnTwo oid owner activated proto side ts as ss cs count ds
    (Add'Artifact'Die dieID, ColumnTwo oid owner activated proto side ts as ss cs count ds) ->
      Right $ ColumnTwo oid owner activated proto side ts as ss cs count (dieID : ds)
    (Remove'Artifact'Die dieID, ColumnTwo oid owner activated proto side ts as ss cs count ds) -> do
      remaining <- removeDieReference dieID oid ds
      Right $ ColumnTwo oid owner activated proto side ts as ss cs count remaining

modifyColumnThree
  :: Artifact'Change
  -> Object (Artifact Column'Three)
  -> Either Transformation'Error (Object (Artifact Column'Three))
modifyColumnThree change artifact =
  case (change, artifact) of
    (Set'Artifact'Activation activated, ColumnThree oid owner _ proto side charge ts as ss cs ult used count ds) ->
      Right $ ColumnThree oid owner activated proto side charge ts as ss cs ult used count ds
    (Set'Artifact'Side side, ColumnThree oid owner activated proto _ charge ts as ss cs ult used count ds) ->
      Right $ ColumnThree oid owner activated proto side charge ts as ss cs ult used count ds
    (Set'Artifact'Counter count, ColumnThree oid owner activated proto side charge ts as ss cs ult used _ ds) ->
      Right $ ColumnThree oid owner activated proto side charge ts as ss cs ult used count ds
    (Add'Artifact'Die dieID, ColumnThree oid owner activated proto side charge ts as ss cs ult used count ds) ->
      Right $ ColumnThree oid owner activated proto side charge ts as ss cs ult used count (dieID : ds)
    (Remove'Artifact'Die dieID, ColumnThree oid owner activated proto side charge ts as ss cs ult used count ds) -> do
      remaining <- removeDieReference dieID oid ds
      Right $ ColumnThree oid owner activated proto side charge ts as ss cs ult used count remaining

replaceObject :: Game'ID -> Game'Object -> Game -> Game
replaceObject objectID replacement game =
  setGameObjects
    ( map
        ( \(storedID, object) ->
            if storedID == objectID
              then (storedID, replacement)
              else (storedID, object)
        )
        game.objects
    )
    game

ensureObjectMissing
  :: Game'ID
  -> Game
  -> Either Transformation'Error ()
ensureObjectMissing objectID game =
  case lookupObject objectID game of
    Nothing -> Right ()
    Just _ -> Left $ Object'Already'Exists objectID

requireObjectExists
  :: Game'ID
  -> Game
  -> Either Transformation'Error ()
requireObjectExists objectID game =
  case lookupObject objectID game of
    Nothing -> Left $ Object'Not'Found objectID
    Just _ -> Right ()

requireUncontainedDie
  :: Game'ID
  -> Game
  -> Either Transformation'Error ()
requireUncontainedDie dieID game = do
  requireObject @Ability'Die dieID game
  case containingObjects dieID game of
    [] -> Right ()
    containerID : _ -> Left $ Die'Already'Contained dieID containerID

containingObjects :: Game'ID -> Game -> [Game'ID]
containingObjects dieID game =
  [ objectID
  | (objectID, object) <- game.objects
  , containsDie dieID object
  ]

containsDie :: Game'ID -> Game'Object -> Bool
containsDie dieID object
  | Just area <- castObject @Area object = dieID `elem` area.dices
  | Just artifact <- castObject @(Artifact Column'One) object =
      dieID `elem` artifact.dices
  | Just artifact <- castObject @(Artifact Column'Two) object =
      dieID `elem` artifact.dices
  | Just artifact <- castObject @(Artifact Column'Three) object =
      dieID `elem` artifact.dices
  | otherwise = False

removeDieReference
  :: Game'ID
  -> Game'ID
  -> [Game'ID]
  -> Either Transformation'Error [Game'ID]
removeDieReference dieID containerID dieIDs
  | dieID `elem` dieIDs =
      Right $ filter (/= dieID) dieIDs
  | otherwise =
      Left $ Die'Not'Contained dieID containerID

setGameObjects :: [(Game'ID, Game'Object)] -> Game -> Game
setGameObjects value (Game _ time nextID seal fall winners rng history) =
  Game value time nextID seal fall winners rng history

setGameTime :: Game'Time -> Game -> Game
setGameTime value (Game objects _ nextID seal fall winners rng history) =
  Game objects value nextID seal fall winners rng history

setGameDustSeal :: Maybe Game'ID -> Game -> Game
setGameDustSeal value (Game objects time nextID _ fall winners rng history) =
  Game objects time nextID value fall winners rng history

setGameDustFall :: Int -> Game -> Game
setGameDustFall value (Game objects time nextID seal _ winners rng history) =
  Game objects time nextID seal value winners rng history

setGameWinners :: Maybe [Game'ID] -> Game -> Game
setGameWinners value (Game objects time nextID seal fall _ rng history) =
  Game objects time nextID seal fall value rng history

setGameNextObjectID :: Game'ID -> Game -> Game
setGameNextObjectID value (Game objects time _ seal fall winners rng history) =
  Game objects time value seal fall winners rng history

setGameRandomGenerator :: StdGen -> Game -> Game
setGameRandomGenerator value (Game objects time nextID seal fall winners _ history) =
  Game objects time nextID seal fall winners value history
