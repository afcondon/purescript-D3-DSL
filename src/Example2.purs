module D3DSL.Example2 where

import Prelude -- (($), (#))
import Data.Array ((..))
import D3DSL

circle :: D3Selection (Array Int)
circle =  DocumentSelect ".svg"
        # SubSelect "circle"
        # Data (1..10)

square :: D3Selection (Array Int)
square =  DocumentSelect ".svg"
        # SubSelect "square"
        # Data (5..20)

awn :: D3Selection (Array Int)
awn = Merge circle square

fub :: D3Selection (Array Int)
fub = awn # Exit # Remove
