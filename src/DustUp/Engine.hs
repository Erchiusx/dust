module DustUp.Engine where

import Control.Monad (guard)
import Control.Monad.Free
import Control.Monad.RWS
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IORef
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
  , during :: ActionM ()
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
  let (new, Engine'State{runtime = runtime'}, l) = runRWS m old $ Engine'State runtime (Pure ())
  -- TODO:
  undefined

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

interpret
  :: ActionD (ActionM ())
  -> EngineM Engine'Status
interpret = undefined

-- interpret action =
--   case action of
--     Deal n Damage To target By source From movement next -> do
--       emit action $
--         Modify'The'Life'Of target By (-n)
--       resolve next
--     Heal n To target By source From movement next -> do
--       emit action $
--         Modify'The'Life'Of target By n
--       resolve next
--     Set'the'counter'on artifact To n From movement next -> do
--       emit action $
--         Set'The'Counter'Of artifact To n
--       resolve next
--     Turn artifact To side From movement next -> do
--       emit action $
--         Set'The'Activated'Side'Of artifact To side
--       resolve next
--     Roll n k -> do
--       dices <- roll'dice n
--       traverse_ (emit action . Roll') dices
--       resolve (k dices)
--     Put dices Onto place From movement next -> do
--       traverse_ (emit action)
--       resolve next
--     Flip dice To face From movement next -> do
--       emit action $
--         Set' dice To face
--       resolve next
--     Remove dice From place From movement next -> do
--       -- 注意：Types 里 ActionD Remove 用 AreaO，
--       -- Transformation Remove' 用 Area，这里类型暂时对不上
--       resolve next
--     Create'Modifier modifier From movement next -> do
--       emit action $
--         Create' modifier
--       resolve next
--     Get'self k -> do
--       rt <- get
--       let p = active'player rt
--       -- 这里还需要通过 Game'ID 找 PlayerO
--       -- 暂时建议写 require'Active'Player
--       self <- require'Active'Player
--       resolve (k self)
--     Get'all'players k -> do
--       rt <- get
--       let ps = players (runtime'state rt)
--       resolve (k ps)
--     Request'movement prompt From player options k -> do
--       let pending =
--             Pending'Movement
--               { pending'prompt = prompt,
--                 pending'player = player,
--                 pending'options = options,
--                 pending'continue = k
--               }

--       modify $ \rt ->
--         rt {runtime'pending = Just pending}

--       pure $
--         Engine'Paused pending

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
