module Main where

import Debug.Trace
import GHC.Wasm.Prim

foreign export javascript test :: IO ()
test :: IO ()
test = trace "some trace" $ print "Hello, Haskell!"

foreign export javascript
  returns :: JSString -> JSString
returns x = toJSString $ fromJSString x

main :: IO ()
main = return ()
