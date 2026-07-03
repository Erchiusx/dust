module DustUp.Initialize where

import Data.Bifunctor (first)
import Data.List (mapAccumL, nub)
import DustUp.Artifacts.Registry
import DustUp.Artifacts.Type
import DustUp.Engine (Engine'Result, runInitialSupply)
import DustUp.Type
import System.Random (Random (randomR), StdGen)

data Player'Loadout = Player'Loadout
  { column'one :: Card'ID
  , column'two :: Card'ID
  , column'three :: Card'ID
  }
  deriving (Show, Eq)

data Initialize'Error
  = Unknown'Column'One Card'ID
  | Unknown'Column'Two Card'ID
  | Unknown'Column'Three Card'ID
  | Duplicate'Artifact Card'ID
  | Invalid'Initial'Game String
  deriving (Show, Eq)

initializeGame
  :: StdGen
  -> Player'Loadout
  -> Player'Loadout
  -> Either Initialize'Error Engine'Result
initializeGame generator firstLoadout secondLoadout = do
  let (firstPlayer, generator') = randomR (0 :: Int, 1) generator
  initializeGameWithFirstPlayer
    generator'
    firstPlayer
    firstLoadout
    secondLoadout

initializeGameWithFirstPlayer
  :: StdGen
  -> Game'ID
  -> Player'Loadout
  -> Player'Loadout
  -> Either Initialize'Error Engine'Result
initializeGameWithFirstPlayer generator firstPlayer firstLoadout secondLoadout = do
  if firstPlayer `elem` [0, 1]
    then Right ()
    else Left $ Invalid'Initial'Game "first player must be 0 or 1"
  validateDistinctArtifacts firstLoadout secondLoadout
  firstDefinitions <- resolveLoadout firstLoadout
  secondDefinitions <- resolveLoadout secondLoadout
  let
    secondPlayer = 1 - firstPlayer
    game =
      buildGame
        generator
        firstPlayer
        firstDefinitions
        secondDefinitions
  first Invalid'Initial'Game $ validateInitialGame game
  pure $ runInitialSupply [firstPlayer, secondPlayer] game

data Resolved'Loadout = Resolved'Loadout
  { one :: Artifact'Definition Column'One
  , two :: Artifact'Definition Column'Two
  , three :: Artifact'Definition Column'Three
  }

resolveLoadout
  :: Player'Loadout
  -> Either Initialize'Error Resolved'Loadout
resolveLoadout loadout = do
  one <-
    maybe
      (Left $ Unknown'Column'One loadout.column'one)
      Right
      (lookupColumnOne loadout.column'one)
  two <-
    maybe
      (Left $ Unknown'Column'Two loadout.column'two)
      Right
      (lookupColumnTwo loadout.column'two)
  three <-
    maybe
      (Left $ Unknown'Column'Three loadout.column'three)
      Right
      (lookupColumnThree loadout.column'three)
  pure $ Resolved'Loadout one two three

validateDistinctArtifacts
  :: Player'Loadout
  -> Player'Loadout
  -> Either Initialize'Error ()
validateDistinctArtifacts firstLoadout secondLoadout =
  case [ cardID
       | cardID <-
           [ firstLoadout.column'one
           , firstLoadout.column'two
           , firstLoadout.column'three
           , secondLoadout.column'one
           , secondLoadout.column'two
           , secondLoadout.column'three
           ]
       , count cardID allCards > 1
       ] of
    duplicate : _ -> Left $ Duplicate'Artifact duplicate
    [] -> Right ()
 where
  allCards =
    [ firstLoadout.column'one
    , firstLoadout.column'two
    , firstLoadout.column'three
    , secondLoadout.column'one
    , secondLoadout.column'two
    , secondLoadout.column'three
    ]
  count value = length . filter (== value)

buildGame
  :: StdGen
  -> Game'ID
  -> Resolved'Loadout
  -> Resolved'Loadout
  -> Game
buildGame generator firstPlayer firstLoadout secondLoadout =
  let
    firstLayout = Player'Layout 0 (2, 3, 4) (5, 6, 7)
    secondLayout = Player'Layout 1 (8, 9, 10) (11, 12, 13)
    (nextID, firstObjects) =
      instantiatePlayer 14 firstLayout firstLoadout
    (finalID, secondObjects) =
      instantiatePlayer nextID secondLayout secondLoadout
    secondPlayer = 1 - firstPlayer
   in
    Game
      { objects = firstObjects <> secondObjects
      , time = Game'Time 0 firstPlayer SupplyPhase
      , next'object'id = finalID
      , dust'seal'holder = Just secondPlayer
      , dust'fall = 0
      , winners = Nothing
      , random'generator = generator
      , history = History []
      }

data Player'Layout = Player'Layout
  { player'id :: Game'ID
  , artifact'ids :: (Game'ID, Game'ID, Game'ID)
  , area'ids :: (Game'ID, Game'ID, Game'ID)
  }

instantiatePlayer
  :: Game'ID
  -> Player'Layout
  -> Resolved'Loadout
  -> (Game'ID, [(Game'ID, Game'Object)])
instantiatePlayer nextID layout loadout =
  let
    (oneID, twoID, threeID) = layout.artifact'ids
    (attackAreaID, defenceAreaID, thoughtAreaID) = layout.area'ids
    oneContext =
      Artifact'Context oneID layout.player'id layout.area'ids
    twoContext =
      Artifact'Context twoID layout.player'id layout.area'ids
    threeContext =
      Artifact'Context threeID layout.player'id layout.area'ids
    onePrototype = loadout.one.build oneContext
    twoPrototype = loadout.two.build twoContext
    threePrototype = loadout.three.build threeContext
    playerPrototype =
      PlayerTemplate onePrototype twoPrototype threePrototype
    (afterOne, oneObject, oneAbilities) =
      instantiateColumnOne nextID layout.player'id oneID onePrototype
    (afterTwo, twoObject, twoAbilities) =
      instantiateColumnTwo afterOne layout.player'id twoID twoPrototype
    (afterThree, threeObject, threeAbilities) =
      instantiateColumnThree afterTwo layout.player'id threeID threePrototype
    playerObject =
      Object $
        Player
          layout.player'id
          playerPrototype
          threePrototype.life
          layout.artifact'ids
          layout.area'ids
    areaObjects =
      [
        ( attackAreaID
        , Object $ Area'Object attackAreaID (Area Attacking) layout.player'id []
        )
      ,
        ( defenceAreaID
        , Object $ Area'Object defenceAreaID (Area Defencing) layout.player'id []
        )
      ,
        ( thoughtAreaID
        , Object $ Area'Object thoughtAreaID (Area Thoughtful) layout.player'id []
        )
      ]
    fixedObjects =
      [ (layout.player'id, playerObject)
      , (oneID, Object oneObject)
      , (twoID, Object twoObject)
      , (threeID, Object threeObject)
      ]
   in
    ( afterThree
    , fixedObjects
        <> areaObjects
        <> oneAbilities
        <> twoAbilities
        <> threeAbilities
    )

instantiateColumnOne
  :: Game'ID
  -> Game'ID
  -> Game'ID
  -> Artifact Column'One
  -> ( Game'ID
     , Object (Artifact Column'One)
     , [(Game'ID, Game'Object)]
     )
instantiateColumnOne nextID ownerID artifactID prototype =
  let
    (afterTriggers, triggerIDs, triggerObjects) =
      instantiateSideAbilities
        nextID
        Triggered'Ability
        prototype.triggers
    (afterActived, activedIDs, activedObjects) =
      instantiateSideAbilities
        afterTriggers
        Actived'Ability
        prototype.actived
    (afterStatic, staticIDs, staticObjects) =
      instantiateSideAbilities
        afterActived
        Static'Ability
        prototype.static
    (afterCharged, chargedIDs, chargedObjects) =
      instantiateSideSingle
        afterStatic
        Charged'Ability
        prototype.charged
    object =
      ColumnOne
        artifactID
        ownerID
        False
        Left'Side
        prototype
        triggerIDs
        activedIDs
        staticIDs
        chargedIDs
        0
        []
   in
    ( afterCharged
    , object
    , triggerObjects <> activedObjects <> staticObjects <> chargedObjects
    )

instantiateColumnTwo
  :: Game'ID
  -> Game'ID
  -> Game'ID
  -> Artifact Column'Two
  -> ( Game'ID
     , Object (Artifact Column'Two)
     , [(Game'ID, Game'Object)]
     )
instantiateColumnTwo nextID ownerID artifactID prototype =
  let
    (afterTriggers, triggerIDs, triggerObjects) =
      instantiateSideAbilities
        nextID
        Triggered'Ability
        prototype.triggers
    (afterActived, activedIDs, activedObjects) =
      instantiateSideAbilities
        afterTriggers
        Actived'Ability
        prototype.actived
    (afterStatic, staticIDs, staticObjects) =
      instantiateSideAbilities
        afterActived
        Static'Ability
        prototype.static
    (afterCharged, chargedIDs, chargedObjects) =
      instantiateSideSingle
        afterStatic
        Charged'Ability
        prototype.charged
    object =
      ColumnTwo
        artifactID
        ownerID
        False
        prototype
        Left'Side
        triggerIDs
        activedIDs
        staticIDs
        chargedIDs
        0
        []
   in
    ( afterCharged
    , object
    , triggerObjects <> activedObjects <> staticObjects <> chargedObjects
    )

instantiateColumnThree
  :: Game'ID
  -> Game'ID
  -> Game'ID
  -> Artifact Column'Three
  -> ( Game'ID
     , Object (Artifact Column'Three)
     , [(Game'ID, Game'Object)]
     )
instantiateColumnThree nextID ownerID artifactID prototype =
  let
    levels = [0 .. prototype.capability]
    (afterTriggers, triggerIDs, triggerObjects) =
      instantiateLevelSideAbilities
        nextID
        levels
        Triggered'Ability
        prototype.triggers
    (afterActived, activedIDs, activedObjects) =
      instantiateLevelSideAbilities
        afterTriggers
        levels
        Actived'Ability
        prototype.actived
    (afterStatic, staticIDs, staticObjects) =
      instantiateLevelSideAbilities
        afterActived
        levels
        Static'Ability
        prototype.static
    (afterCharged, chargedIDs, chargedObjects) =
      instantiateLevelSideSingle
        afterStatic
        levels
        Charged'Ability
        prototype.charged
    ultimateID = afterCharged
    ultimateObject =
      ( ultimateID
      , Object $
          Ability'Object
            ultimateID
            (Actived'Ability prototype.ultimate)
            False
      )
    object =
      ColumnThree
        artifactID
        ownerID
        False
        prototype
        Left'Side
        0
        triggerIDs
        activedIDs
        staticIDs
        chargedIDs
        ultimateID
        False
        []
   in
    ( ultimateID + 1
    , object
    , triggerObjects
        <> activedObjects
        <> staticObjects
        <> chargedObjects
        <> [ultimateObject]
    )

instantiateSideAbilities
  :: Game'ID
  -> (Ability t -> Ability'Prototype)
  -> (Side -> [Ability t])
  -> ( Game'ID
     , Side -> [Game'ID]
     , [(Game'ID, Game'Object)]
     )
instantiateSideAbilities nextID wrap abilities =
  let
    (afterLeft, leftIDs, leftObjects) =
      instantiateAbilities nextID wrap $ abilities Left'Side
    (afterRight, rightIDs, rightObjects) =
      instantiateAbilities afterLeft wrap $ abilities Right'Side
    ids side =
      case side of
        Left'Side -> leftIDs
        Right'Side -> rightIDs
   in
    (afterRight, ids, leftObjects <> rightObjects)

instantiateSideSingle
  :: Game'ID
  -> (Ability t -> Ability'Prototype)
  -> (Side -> Ability t)
  -> ( Game'ID
     , Side -> Game'ID
     , [(Game'ID, Game'Object)]
     )
instantiateSideSingle nextID wrap ability =
  let
    (afterLeft, leftID, leftObject) =
      instantiateAbility nextID wrap $ ability Left'Side
    (afterRight, rightID, rightObject) =
      instantiateAbility afterLeft wrap $ ability Right'Side
    ids side =
      case side of
        Left'Side -> leftID
        Right'Side -> rightID
   in
    (afterRight, ids, [leftObject, rightObject])

instantiateLevelSideAbilities
  :: Game'ID
  -> [Charge'Level]
  -> (Ability t -> Ability'Prototype)
  -> (Charge'Level -> Side -> [Ability t])
  -> ( Game'ID
     , Charge'Level -> Side -> [Game'ID]
     , [(Game'ID, Game'Object)]
     )
instantiateLevelSideAbilities nextID levels wrap abilities =
  let
    (finalID, entries) =
      mapAccumL
        ( \currentID (level, side) ->
            let (next, ids, objects) =
                  instantiateAbilities
                    currentID
                    wrap
                    (abilities level side)
             in (next, ((level, side), ids, objects))
        )
        nextID
        [(level, side) | level <- levels, side <- [Left'Side, Right'Side]]
    ids level side =
      maybe [] (\(_, found, _) -> found) $
        findEntry (level, side) entries
    objects = concatMap (\(_, _, values) -> values) entries
   in
    (finalID, ids, objects)

instantiateLevelSideSingle
  :: Game'ID
  -> [Charge'Level]
  -> (Ability t -> Ability'Prototype)
  -> (Charge'Level -> Side -> Ability t)
  -> ( Game'ID
     , Charge'Level -> Side -> Game'ID
     , [(Game'ID, Game'Object)]
     )
instantiateLevelSideSingle nextID levels wrap ability =
  let
    (finalID, entries) =
      mapAccumL
        ( \currentID (level, side) ->
            let (next, abilityID, object) =
                  instantiateAbility
                    currentID
                    wrap
                    (ability level side)
             in (next, ((level, side), abilityID, object))
        )
        nextID
        [(level, side) | level <- levels, side <- [Left'Side, Right'Side]]
    ids level side =
      maybe (-1) (\(_, found, _) -> found) $
        findEntry (level, side) entries
    objects = map (\(_, _, object) -> object) entries
   in
    (finalID, ids, objects)

instantiateAbilities
  :: Game'ID
  -> (Ability t -> Ability'Prototype)
  -> [Ability t]
  -> (Game'ID, [Game'ID], [(Game'ID, Game'Object)])
instantiateAbilities nextID wrap abilities =
  let (finalID, entries) =
        mapAccumL
          ( \currentID ability ->
              let (next, abilityID, object) =
                    instantiateAbility currentID wrap ability
               in (next, (abilityID, object))
          )
          nextID
          abilities
   in (finalID, map fst entries, map snd entries)

instantiateAbility
  :: Game'ID
  -> (Ability t -> Ability'Prototype)
  -> Ability t
  -> (Game'ID, Game'ID, (Game'ID, Game'Object))
instantiateAbility abilityID wrap ability =
  ( abilityID + 1
  , abilityID
  ,
    ( abilityID
    , Object $ Ability'Object abilityID (wrap ability) False
    )
  )

findEntry
  :: Eq key
  => key
  -> [(key, value, object)]
  -> Maybe (key, value, object)
findEntry wanted =
  foldr
    (\entry@(key, _, _) found -> if key == wanted then Just entry else found)
    Nothing

validateInitialGame :: Game -> Either String ()
validateInitialGame game = do
  let objectIDs = map fst game.objects
  if length objectIDs == length (nub objectIDs)
    then Right ()
    else Left "duplicate game object IDs"
  mapM_ validatePlayer [0, 1]
 where
  validatePlayer playerID =
    case lookup playerID game.objects >>= castObject @Player of
      Nothing -> Left $ "missing player " <> show playerID
      Just (Player _ _ _ artifactIDs areaIDs) -> do
        mapM_ requireObjectID $
          tupleToList artifactIDs <> tupleToList areaIDs
  requireObjectID objectID =
    case lookup objectID game.objects of
      Nothing -> Left $ "missing referenced object " <> show objectID
      Just _ -> Right ()

tupleToList :: (a, a, a) -> [a]
tupleToList (one, two, three) = [one, two, three]
