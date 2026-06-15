{- HLINT ignore "Redundant id" -}
module DustUp.Engine where

import Control.Lens
import Control.Monad (forM, forM_, guard, replicateM)
import Control.Monad.Free
import Control.Monad.RWS
import Data.Foldable (traverse_)
import Data.Generics.Product.Fields
import Data.IORef
import Data.List (find)
import Data.Maybe
import Data.Monoid
import DustUp.LiteralWords
import DustUp.Types
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import System.Random

--------------------------------------------------------------------------------
-- Runtime
--------------------------------------------------------------------------------

data Runtime
  = Runtime
  { runtime'state :: Game'State
  , runtime'time :: Game'Time
  , runtime'pending :: Maybe Pending'Input
  , active'player :: Game'ID
  }
  deriving Generic

data Pending'Input
  = Pending'Movement
  { pending'prompt :: String
  , pending'player :: PlayerO
  , pending'options :: Movement'Options
  , pending'continue :: Movement -> ActionM ()
  }

data Engine'Status
  = Engine'Done
  | Engine'Paused Pending'Input

data Engine'State
  = Engine'State
  { runtime :: Runtime
  , queue :: ActionM ()
  }
  deriving Generic

--------------------------------------------------------------------------------
-- Global wasm-instance state
--------------------------------------------------------------------------------

runtime'Ref
  :: IORef (Maybe Runtime)
{-# NOINLINE runtime'Ref #-}
runtime'Ref = unsafePerformIO $ newIORef Nothing

read'Runtime
  :: IO (Maybe Runtime)
read'Runtime = readIORef runtime'Ref

write'Runtime
  :: Runtime
  -> IO ()
write'Runtime = writeIORef runtime'Ref . Just

data Event = Event
  { transformation :: Transformation
  , during :: ActionD (ActionM ())
  }

type EngineM =
  RWS
    Engine'Status
    [Event]
    Engine'State

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

initialize'Game
  :: Player
  -> Player
  -> StdGen
  -> IO ()
initialize'Game p1 p2 rng = do
  writeIORef runtime'Ref $
    Just $
      init'Runtime p1 p2 rng

--------------------------------------------------------------------------------
-- Runtime initialization
--------------------------------------------------------------------------------

init'Runtime
  :: Player
  -> Player
  -> StdGen
  -> Runtime
init'Runtime p1 p2 rng =
  let game = init'Game p1 p2 rng
   in Runtime game initial'Time Nothing 0

initial'Time
  :: Game'Time
initial'Time =
  Game'Time
    { round = 0
    , player = 0
    , phase = UPKEEP
    }

--------------------------------------------------------------------------------
-- Engine entrypoints inside EngineM
--------------------------------------------------------------------------------

runEngine
  :: Engine'Status
  -> EngineM Engine'Status
  -> IO Engine'Status
runEngine old m = do
  Just runtime <- read'Runtime
  let (new, Engine'State{runtime = runtime', queue}, l) = runRWS m old $ Engine'State runtime (Pure ())
  let (triggered, transformed) = runtime' `apply'transformations` l
  write'Runtime transformed
  let next = queue >> triggered
  case next of
    Pure () ->
      return new
    _ -> runEngine new (resolve next)

engine'game :: Lens' Engine'State Game'State
engine'game =
  field @"runtime"
    . field @"runtime'state"

assign'object :: EngineM Int
assign'object = do
  oid <- use $ engine'game . field @"game'object'count"
  engine'game . field @"game'object'count" .= oid + 1
  pure oid

type CheckerM = RWS Runtime (ActionM (), Dual (Endo Runtime)) ()
runChecker :: CheckerM () -> Runtime -> (ActionM (), Runtime)
runChecker m r =
  let ((), (), (continuation, Dual (Endo rf))) = runRWS m r ()
   in (continuation, rf r)

trigger' :: Game'State -> Event -> ActionM ()
trigger' = undefined

-- trigger' game@Game'State{triggers} Event{transformation, during} =
--   forM_
--     (map (.ability) triggers)
--     $ (.trigger game during transformation)

state'transform :: Transformation -> Dual (Endo Runtime)
state'transform =
  Dual . Endo . \case
    Modify'The'Life'Of Player{oid} By n ->
      runtime'players
        . with'oid oid
        . field' @"life"
        %~ (+ n)
    Set'The'Counter'Of Artifact{oid} To n ->
      runtime'artifacts
        . with'oid oid
        %~ ( (field' @"counters" .~)
               =<< (min <$> (.template.cap) <*> pure (max 0 n))
           )
    Set'The'Activated'Side'Of Artifact{oid} To side ->
      runtime'artifacts
        . with'oid oid
        . field' @"actived'side"
        ?~ side
    Give'Dust'Seal To player ->
      runtime'dust'seal
        ?~ player
    Remove'Dust'Seal{} ->
      runtime'dust'seal
        .~ Nothing
    Set' Dice{oid} To dice ->
      runtime'areas
        . field' @"area"
        . traversed
        . with'oid oid
        . field' @"dice"
        .~ dice
    Create'Modifier' modifier ->
      runtime'modifiers
        %~ (modifier :)
    Create'Trigger' trigger ->
      runtime'triggers
        %~ (trigger :)
    Put' dice@Dice{oid} Onto (Left Artifact{oid = aoid}) ->
      runtime'players
        %~ ( player'artifacts
               . with'oid aoid
               . artifact'dices
               %~ (dice :)
           )
          . ( player'artifacts
                . artifact'dices
                %~ without'oid oid
            )
          . ( player'areas
                . area'dices
                %~ without'oid oid
            )
    Put' dice@Dice{oid} Onto (Right Area{oid = aoid}) ->
      runtime'players
        %~ ( player'areas
               . with'oid aoid
               . area'dices
               %~ (dice :)
           )
          . ( player'areas
                . area'dices
                %~ without'oid oid
            )
          . ( player'artifacts
                . artifact'dices
                %~ without'oid oid
            )
    Remove' Dice{oid} From (Left Artifact{oid = aoid}) ->
      runtime'artifacts
        . with'oid aoid
        . artifact'dices
        %~ without'oid oid
    Remove' Dice{oid} From (Right Area{oid = aoid}) ->
      runtime'areas
        . with'oid aoid
        . area'dices
        %~ without'oid oid
    Time'Advance ->
      \runtime ->
        let
          time = runtime.runtime'time
          players = runtime.runtime'state.players
          player'ids = map (.oid) players
          phase'wraps =
            time.phase == maxBound
          phase' =
            if phase'wraps
              then minBound
              else succ time.phase
          player'wraps =
            phase'wraps
              && time.player == last player'ids
          player' =
            if phase'wraps
              then
                if player'wraps
                  then head player'ids
                  else (player'ids !!) $ (+ 1) (fromJust (find (== time.player) player'ids))
              else time.player
          round' =
            if player'wraps
              then time.round + 1
              else time.round
         in
          runtime
            & runtime'time
              %~ (field @"phase" .~ phase')
                . (field @"player" .~ player')
                . (field @"round" .~ round')
            & runtime'active'player .~ player'
 where
  runtime'state = field @"runtime'state"
  runtime'time = field @"runtime'time"
  runtime'active'player = field @"active'player"
  runtime'players =
    runtime'state
      . field @"players"
      . traversed
  runtime'artifacts =
    runtime'players
      . player'artifacts
  runtime'areas =
    runtime'players
      . player'areas
  runtime'dust'seal =
    runtime'state
      . field @"dust'seal"
  runtime'modifiers =
    runtime'state
      . field @"modifiers"
  runtime'triggers =
    runtime'state
      . field @"triggers"
  player'artifacts =
    field' @"artifacts"
      . each
  player'areas =
    field' @"areas"
      . each
  artifact'dices =
    field' @"dices"
  area'dices =
    field' @"area"
  with'oid oid =
    filtered ((== oid) . (.oid))
  without'oid oid =
    filter ((/= oid) . (.oid))

-- state'check :: Event -> RWS Runtime (ActionM (), Dual (Endo Runtime)) () ()
-- state'check event@Event{transformation} = do
--   state <- asks runtime'state
--   let
--     triggered = trigger state event
--     transform = state'transform transformation
--   tell (triggered, transform)

-- apply'transformations
--   :: Runtime -> [Event] -> (ActionM (), Runtime)
-- apply'transformations runtime [] = (Pure (), runtime)
-- apply'transformations runtime events =
--   ((\(Dual patch) -> appEndo patch runtime) <$>) $
--     (\((), (), a) -> a) $
--       runRWS @Runtime @(ActionM (), Dual (Endo Runtime)) @()
--         (forM_ events state'check)
--         runtime
--         ()
state'check :: Event -> CheckerM ()
state'check event@Event{transformation} = do
  state <- asks runtime'state
  let
    triggered = trigger' state event
    transform = state'transform transformation
  tell (triggered, transform)

apply'transformations
  :: Runtime -> [Event] -> (ActionM (), Runtime)
apply'transformations runtime events =
  runChecker (traverse_ state'check events) runtime

--------------------------------------------------------------------------------
-- Transpile: Movement -> ActionM
--------------------------------------------------------------------------------

-- transpile
--   :: Movement
--   -> EngineM (ActionM ())

--------------------------------------------------------------------------------
-- Resolve: ActionM -> Transformation log
--------------------------------------------------------------------------------

resolve
  :: ActionM ()
  -> EngineM Engine'Status
resolve action =
  case action of
    Pure () ->
      pure Engine'Done
    Free actionD ->
      interpret actionD

apply'modifiers
  :: Transformation
  -> ActionD (ActionM ())
  -> EngineM Transformation
apply'modifiers raw action = do
  game <- use engine'game
  return $
    foldl'
      (&)
      raw
      [ applyModifier modifier game action
      | Modifier{modifier} <- game.modifiers
      ]

tells :: MonadWriter [w] m => w -> m ()
tells = tell . (: [])

interpret
  :: ActionD (ActionM ())
  -> EngineM Engine'Status
interpret action = case action of
  Deal n Damage To player By _ From _ continuation -> do
    let raw = Modify'The'Life'Of player By (-n)
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Heal n To player By _ From _ continuation -> do
    let raw = Modify'The'Life'Of player By n
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Set'the'counter'on artifact To n From _ continuation -> do
    let raw = Set'The'Counter'Of artifact To n
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Turn artifact To side From _ continuation -> do
    let raw = Set'The'Activated'Side'Of artifact To side
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Roll n continuation -> do
    objectIds <- replicateM n assign'object
    game <- use engine'game
    let rng = game.rng
    let (rns, rng') = uniformListR @Integer n (0, 5) rng
    let raw'dices = map (fromInteger @Dice) rns
    let dices = zipWith Dice objectIds raw'dices
    engine'game . field @"rng" .= rng'
    resolve (continuation dices)
  Put_ dices Onto target From _ continuation -> do
    forM
      dices
      ( \dice -> do
          eventtrans <- apply'modifiers (Put' dice Onto target) action
          return $
            Event
              { transformation = eventtrans
              , during = action
              }
      )
      >>= tell
    resolve continuation
  Flip object To dice From _ continuation -> do
    let raw = Set' object To dice
    modified <- apply'modifiers raw action
    tells
      Event
        { transformation = modified
        , during = action
        }
    resolve continuation
  Remove dices From target From _ continuation -> do
    forM
      dices
      ( \dice -> do
          eventtrans <- apply'modifiers (Remove' dice From target) action
          return $
            Event
              { transformation = eventtrans
              , during = action
              }
      )
      >>= tell
    resolve continuation
  Create'Modifier modifier From _ continuation -> do
    oid <- assign'object
    let
      object = Modifier oid modifier
      raw = Create'Modifier' object
    modified <- apply'modifiers raw action
    tells
      Event
        { transformation = modified
        , during = action
        }
    resolve (continuation object)
  Create'Trigger ability From _ continuation -> do
    oid <- assign'object
    case ability of
      trigger@Triggered{} -> do
        let
          object = Trigger oid trigger
          raw = Create'Trigger' object
        modified <- apply'modifiers raw action
        tells
          Event
            { transformation = modified
            , during = action
            }
        resolve (continuation $ Just object)
      _ -> resolve (continuation Nothing)
  Get'active'player continuation -> do
    Engine'State{runtime} <- get
    let Runtime{active'player, runtime'state = game} = runtime
    let active'player' =
          fromMaybe (head game.players) $
            find (\Player{oid} -> oid == active'player) game.players
    resolve (continuation active'player')
  Get'all'players continuation -> do
    Engine'State{runtime} <- get
    let Runtime{runtime'state = Game'State{players}} = runtime
    resolve (continuation players)
  Request'movement prompt From player options continuation -> do
    return $ Engine'Paused $ Pending'Movement prompt player options continuation

--------------------------------------------------------------------------------
-- Modifier / Condition
--------------------------------------------------------------------------------

-- apply'Modifiers
--   :: Game'State
--   -> ActionD a
--   -> Transformation
--   -> Transformation

-- collect'Triggers
--   :: Game'State
--   -> ActionD a
--   -> Transformation
--   -> [Movement]

-- resolve'Triggers
--   :: [Movement]
--   -> EngineM Engine'Status

--------------------------------------------------------------------------------
-- Maintain
--------------------------------------------------------------------------------

-- maintain
--   :: Game'State
--   -> [Transformation]
--   -> Game'State

-- maintain'Runtime
--   :: Runtime
--   -> [Transformation]
--   -> Runtime
