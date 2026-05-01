module DustUp.Engine where

import Control.Monad (guard, replicateM)
import Control.Monad.Free
import Control.Monad.RWS
import Data.Function ((&))
import Data.IORef
import DustUp.LiteralWords
import DustUp.Types
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
  write'Runtime $ runtime' `apply'transformations` l
  case queue of
    Pure () ->
      return new
    _ -> runEngine new (resolve queue)

assign'object :: EngineM Int
assign'object = do
  engine@Engine'State
    { runtime = old@Runtime{runtime'state = state@Game'State{game'object'count}}
    } <-
    get
  let count = game'object'count + 1
  put $
    engine
      { runtime =
          old
            { runtime'state =
                state
                  { game'object'count = count
                  }
            }
      }
  return game'object'count

-- TODO:
apply'transformations
  :: Runtime -> [Event] -> Runtime
apply'transformations = undefined

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
  Engine'State{runtime} <- get
  let Runtime{runtime'state = state@Game'State{modifiers}} = runtime
  return $
    foldl'
      (&)
      raw
      [ modifier state action
      | Modifier modifier <- modifiers
      ]

interpret
  :: ActionD (ActionM ())
  -> EngineM Engine'Status
interpret action = case action of
  Deal n Damage To player By source From movement continuation -> do
    let raw = Modify'The'Life'Of player By (-n)
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Heal n To player By source From movement continuation -> do
    let raw = Modify'The'Life'Of player By n
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Set'the'counter'on artifact To n From movement continuation -> do
    let raw = Set'The'Counter'Of artifact To n
    modified <- apply'modifiers raw action
    tell
      [ Event
          { transformation = modified
          , during = action
          }
      ]
    resolve continuation
  Turn artifact To side From movement continuation -> do
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
    state <- get
    let runtime = state.runtime
    let game = runtime.runtime'state
    let rng = game.rng
    let (rns, rng') = uniformListR @Integer n (0, 5) rng
    let raw'dices = map (fromInteger @Dice) rns
    let dices = zipWith Dice objectIds raw'dices
    put $
      state
        { runtime =
            runtime
              { runtime'state =
                  game
                    { rng = rng'
                    }
              }
        }
    resolve (continuation dices)
  Put_ dices Onto (Left artifact) From movement continuation -> do
    -- TODO: keep inplementing action resolvement
    undefined

emit
  :: ActionD a
  -> Transformation
  -> EngineM ()
emit = undefined

-- emit action transformation = do
--   rt <- get

--   let st =
--         runtime'state rt

--       transformation' =
--         apply'Modifiers st action transformation

--       rt' =
--         maintain'Runtime rt [transformation']

--   tell [transformation']
--   Control.Monad.State.Strict.put rt'
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

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

-- advance'Time
--   :: Game'Time
--   -> Game'Time
