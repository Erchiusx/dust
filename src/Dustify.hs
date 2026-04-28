module Main where

import Debug.Trace

-- import GHC.Wasm.Prim
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import GHC.IO (unsafePerformIO)

state :: IORef Int
state = unsafePerformIO (newIORef 0)

foreign export javascript test :: IO Int
test :: IO Int
test = do
  r <- readIORef state
  writeIORef state (r + 1)
  return r

foreign export javascript returns :: IO Int
returns :: IO Int
returns = readIORef state

main :: IO ()
main = return ()
