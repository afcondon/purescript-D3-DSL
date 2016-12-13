module Main where

import Prelude (Unit, bind, show, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import D3DSL.Example2 (fub)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show fub
  log "hello sailor"
