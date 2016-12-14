module GUPIII where

import D3DSL
import Prelude (negate, show, (#), ($), (*), (/), (<>))
-- | purely declarative code to set up the computation
t :: D3Transition
t = SimpleTransition $ MS 750

svg :: ∀ d i. D3Selection d i
svg    = DocumentSelect "svg"

width :: Number
width  = getAttrN svg "width"

height :: Number
height = getAttrN svg "height"

g :: ∀ d i. D3Selection d i
g      = svg # Append SvgGroup
             # Attrs [ Transform $ "translate(32," <> show (height / 2.0) <> ")"]

join :: Array Char -> D3Selection Char Char
join myData = g # SelectAll "text"
                # DataAI myData \d -> d

exit :: D3Selection Char Char -> D3Selection Char Char
exit s = s # Exit
           # Attrs [ Class $ V "exit" ]
           # Transition t
           # Attrs [ Y $ V 60.0
                   , Style "fill-opacity" $ V invisible ]
           # Remove

update :: D3Selection Char Char -> D3Selection Char Char
update s = s # Attrs [ Class $ V "update"
                     , Y     $ V 0.0
                     , Style "fill-opacity" $ V opaque ]
             # Transition t
             # Attrs [ X $ F $ Lambda2 \d i -> i * 32.0 ]

enter :: D3Selection Char Char -> D3Selection Char Char
enter s = s # Enter
            # Append SvgText
            # Attrs [ Class $ V "enter"
                    , DY $ V ".35em"
                    , Y  $ V (-60.0)
                    , X  $ F $ Lambda2 \d i -> i * 32.0
                    , Style "fill-opacity" $ V invisible ]
            # Attrs [ Text $ F $ Lambda1 \d -> show d ]
            # Transition t
            # Attrs [ Y $ V 0.0
                    , Style "fill-opacity" $ V opaque ]

-- now all the action happens when we run all these pure computations inside the
-- D3 effect
-- doUpdate :: ∀ e. Array Char -> Eff (d3::D3, dom::DOM|eff) Unit
-- doUpdate myData = do
--   s <- runD3 join myData
--   runD3 exit s
--   runD3 update text
--   runD3 enter
