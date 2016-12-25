module GUPIII where

import D3DSL.Base
import Data.List
import Control.Monad.List.Trans (cons)
import Data.Foldable (class Foldable, foldl)
import Data.String (toCharArray)
import Prelude (append, negate, show, (#), ($), (*), (/), (<>))

type D3S d i = List (D3Selection d i)

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

-- | purely declarative code to set up the computation
width :: Number
width  = 950.0  -- getAttrN svg "width"

height :: Number
height = 600.0  -- getAttrN svg "height"

t :: D3Transition
t = SimpleTransition $ MS 750

-- svg :: ∀ d i. D3Selection Char Char
svg :: forall i d. D3Selection d i
svg    = DocumentSelect "svg"

join :: forall t13. t13 -> D3S Char Char
join myData = fromFoldable [ svg
                           , SelectAll "text"
                           , DataAI (toCharArray "this is this") (\d -> d)]

-- exit :: D3Selection Char Char -> D3Selection Char Char
exit :: D3S Char Char -> D3S Char Char
exit s = s <> fromFoldable [ Exit
                  , Attrs [ Class $ V "exit" ]
                  , Transition t
                  , Attrs [ Y $ V 60.0
                          , Style "fill-opacity" $ V invisible ]
                  , Remove ]

update :: D3S Char Char -> D3S Char Char
update s = s <> fromFoldable [ Attrs [ Class $ V "update"
                                    , Y     $ V 0.0
                                    , Style "fill-opacity" $ V opaque ]
                    , Transition t
                    , Attrs [ X $ F $ Lambda2 \d i -> i * 32.0 ] ]

enter :: D3S Char Char -> D3S Char Char
enter s = s <> fromFoldable [ Enter
                    , Append SvgText
                    , Attrs [ Class $ V "enter"
                                    , DY $ V ".35em"
                                    , Y  $ V (-60.0)
                                    , X  $ F $ Lambda2 \d i -> i * 32.0
                                    , Style "fill-opacity" $ V invisible ]
                    , Attrs [ Text $ F $ Lambda1 \d -> show d ]
                    , Transition t
                    , Attrs [ Y $ V 0.0
                            , Style "fill-opacity" $ V opaque ] ]

-- now all the action happens when we run all these pure computations inside the
-- D3 effect
-- doUpdate :: ∀ e. Array Char -> Eff (d3::D3, dom::DOM|eff) Unit
-- doUpdate myData = do
--   s <- runD3 join myData
--   runD3 exit s
--   runD3 update text
--   runD3 enter
