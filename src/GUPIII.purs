module GUPIII where

import D3DSL
import Data.Array (updateAt, (..))
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Prelude (Unit, (#), ($))

-- | purely declarative code to set up the computation
t      = d3Transition [ Duration 750 ]
svg    = DocumentSelect "svg"
width  = getAttr svg "width"
height = getAttr svg "height"
g      = svg # Append Group [ Transform $ "translate(32," <> show (height / 2) <> ")"]

join :: Array Char -> D3Selection Char
join myData = g # SelectAll "text"
                # DataI myData \d -> d

exit :: D3Selection Char -> D3Selection Char
exit s = s # Exit [ Class "exit" ]
           # Transition t [ Y $ V 60.0
                          , Style "fill-opacity" $ V invisible ]
           # Remove

update :: D3Selection Char -> D3Selection Char
update s = s # Update [ Class "update"
                      # Y 0.0
                      # Style "fill-opacity" $ V opaque ]
             # Transition t [ X $ Lambda2 \d i -> i * 32 ]

enter :: D3Selection Char -> D3Selection Char
enter s = s # Enter
            # Append "text" [ Class "enter"
                            # DY $ V ".35em"
                            # Y  $ V -60.0
                            # X  $ Lambda2 \d i -> i * 32 ]
                            # Style "fill-opacity" $ V invisible ]
            # Text $ Lambda1 \d -> d
            # Transition t [ Y $ V 0.0
                           # Style "fill-opacity" $ V 1.0 ]

-- now all the action happens when we run all these pure computations inside the
-- D3 effect
doUpdate :: ∀ e. Array Char -> Eff (d3::D3, dom::DOM|eff) Unit
doUpdate myData = do
  s <- runD3 join myData
  runD3 exit s
  runD3 update text
  runD3 enter
