module Main where

import Prelude (Unit, bind, show, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.String
import D3DSL.Example (remove, circle)
import GUPIII

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show (update $ join $ toCharArray "this is this")
  log $ show circle
  log $ show remove
  log "hello sailor"
