module Main where

import Prelude (Unit, bind, show, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.String (toCharArray)

import D3DSL.Base (D3)
import DOM (DOM)
import GUPIII (enter, exit, join, update)

main :: forall e. Eff (d3::D3, dom::DOM, console :: CONSOLE | e) Unit
main = do
    -- doUpdate (toCharArray "this is this")
    log $ show (join   $ toCharArray "this is this")
    log $ show (update $ join $ toCharArray "this is this")
    log $ show (exit   $ join $ toCharArray "this is this")
    log $ show (enter  $ join $ toCharArray "this is this")

{-
Attrs: [X Lambda]
Transition
Attrs: [Class Value,Y Value,Style fill-opacity Value]
Data ['t','h','i','s',' ','i','s',' ','t','h','i','s']
    bound to SelectAll "text" in
    Attrs: [Transform(translate(32,300.0))]
    Append Group to
        DocumentSelect  "svg"

  text.exit()
      .attr("class", "exit")
    .transition(t)
      .attr("y", 60)
      .style("fill-opacity", 1e-6)
      .remove();


Remove
Attrs: [Y Value,Style fill-opacity Value]
Transition
Attrs: [Class Value]
Exit
Data ['t','h','i','s',' ','i','s',' ','t','h','i','s']
    bound to SelectAll "text" in
    Attrs: [Transform(translate(32,300.0))]
    Append Group to
        DocumentSelect  "svg"

Attrs: [Y Value,Style fill-opacity Value]
Transition
Attrs: [Text Lambda]
Attrs: [Class Value,DY Value,Y Value,X Lambda,Style fill-opacity Value]
Append Text to
    Enter
    Data ['t','h','i','s',' ','i','s',' ','t','h','i','s']
        bound to SelectAll "text" in
        Attrs: [Transform(translate(32,300.0))]
        Append Group to
            DocumentSelect  "svg"
-}
