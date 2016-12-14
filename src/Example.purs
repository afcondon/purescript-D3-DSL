module D3DSL.Example where

import Prelude ((#), ($))
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.Array ((..))
import D3DSL

-- simple example with Array Int

circle :: D3Selection Int
circle =  DocumentSelect ".svg"
        # SubSelect "circle"
        # Data (1..10)
        # Append (Circle [ CX $ V 5.0
                         , CY $ F $ Lambda1 \d -> 10.0
                         , R  $ F $ Lambda2 \d i -> 20.0 ])

square :: D3Selection Int
square =  DocumentSelect ".svg"
        # SubSelect "square"
        # Data (5..20)
        # Append (Rect [ Height $ V 5.0
                       , Width  $ F $ Lambda1 \d -> 10.0
                       , Style "opacity" $ V "1.0" ])

remove :: D3Selection Int
remove = (Merge circle square) # Exit # Remove


-- | examples of helper / lambda functions
-- | let's have a data type
type MyData = { x :: Number
              , y :: Number
              , radius :: Number
              , something :: Number }


hoy :: ValueOrCallback MyData Number
hoy = V 5.0

ist :: ValueOrCallback Char Number
ist = F $ Lambda1 \d -> toNumber $ toCharCode d

jud :: ∀ d. D3ElementType d
jud = Circle []

kef :: D3ElementType MyData
kef =  Circle [CX hoy
             , CY (F $ Lambda1 \d -> d.y)
             , R  (F $ Lambda2 \d i -> d.something)]
