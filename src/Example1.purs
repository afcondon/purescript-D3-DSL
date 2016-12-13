module D3DSL.Example1 where

import Prelude (($))
import Data.Int (toNumber)
import Data.Char (toCharCode)
import D3DSL

type MyData = { x :: Number, y :: Number, radius :: Number, something :: Number }

-- | examples
hoy :: ValueOrCallback MyData Number
hoy = V 5.0

ist :: ValueOrCallback Char Number
ist = CB $ Lambda \d -> toNumber $ toCharCode d

jud = Circle []

kef =  Circle [CX hoy
             , CY (CB $ Lambda \d -> d.y)
             , R  (CB $ DIfn \d i -> d.something)]
