module D3DSL.Example where

import Prelude ((#), ($))
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.Array ((..))
import D3DSL

-- | let's have a data type
type MyData = { x :: Number
              , y :: Number
              , radius :: Number
              , something :: Number }


circle :: D3Selection (Array Int)
circle =  DocumentSelect ".svg"
        # SubSelect "circle"
        # Data (1..10)
        # Append (Circle [ CX $ V 5.0
                         , CY $ CB $ Lambda \d -> 10.0
                         , R  $ CB $ DIfn \d i -> 20.0 ])

square :: D3Selection (Array Int)
square =  DocumentSelect ".svg"
        # SubSelect "square"
        # Data (5..20)
        # Append (Rect [ Height $ V 5.0
                       , Width  $ CB $ Lambda \d -> 10.0
                       , Style "opacity" $ V "1.0" ])

fub :: D3Selection (Array Int)
fub = (Merge circle square) # Exit # Remove


-- | examples of helper / lambda functions

hoy :: ValueOrCallback MyData Number
hoy = V 5.0

ist :: ValueOrCallback Char Number
ist = CB $ Lambda \d -> toNumber $ toCharCode d

jud :: ∀ d. D3ElementType d
jud = Circle []

kef :: D3ElementType MyData
kef =  Circle [CX hoy
             , CY (CB $ Lambda \d -> d.y)
             , R  (CB $ DIfn \d i -> d.something)]
