module GUPIII where

import Control.Monad.Eff (Eff)
import D3DSL.Base (Attr(..), Callback(..), D3, D3Action(..), D3DocSelect(..), D3ElementType(..), D3S, D3Transition(..), Duration(..), ValueOrCallback(..), invisible, opaque)
import D3DSL.Eval (d3s, d3InitEval, d3EvalActions)
import DOM (DOM)
import Prelude (Unit, bind, negate, pure, show, unit, ($), (*), (<>))

-- | purely declarative code to set up the computation
width :: Number
width  = 950.0  -- getAttrN svg "width"

height :: Number
height = 600.0  -- getAttrN svg "height"

t :: D3Transition
t = SimpleTransition $ MS 750

-- svg :: ∀ d i. D3Action Char Char
svg :: forall i d. D3DocSelect d i
svg    = DocumentSelect "svg"

join :: Array Char -> D3S Char Char
join myData  = d3s [ SelectAll "text"
                   , DataAI myData (\d -> d)]

-- exit :: D3Action Char Char -> D3Action Char Char
exit :: D3S Char Char -> D3S Char Char
exit s = s <> d3s [ Exit
                  , Attrs [ Class $ V "exit" ]
                  , Transition t
                  , Attrs [ Y $ V 60.0
                          , Style "fill-opacity" $ V invisible ]
                  , Remove ]

update :: D3S Char Char -> D3S Char Char
update s = s <> d3s [ Attrs [ Class $ V "update"
                                    , Y     $ V 0.0
                                    , Style "fill-opacity" $ V opaque ]
                    , Transition t
                    , Attrs [ X $ F $ Lambda2 \d i -> i * 32.0 ] ]

enter :: D3S Char Char -> D3S Char Char
enter s = s <> d3s [ Enter
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
doUpdate :: ∀ e. Array Char -> Eff (d3::D3, dom::DOM|e) Unit
doUpdate myData = do
    possSelection <- d3InitEval svg
    d3EvalActions possSelection (join myData)
    pure unit
