module D3DSL where

import Prelude (class Show, show, (<>))
import DOM.Event.Types (Event)
import Color (Color)

type Selector = String
foreign import data Peers :: *        -- a D3Selection that's passed back in some callbacks
foreign import data DomElement :: *   -- the `this` pointer in a callback, DOM element receiving an event

data Callback d b =   Lambda (d -> b)
                    | DIfn   (d -> Number -> b)
                    | DINEfn (d -> Number -> Peers -> DomElement -> b)

data ValueOrCallback d b =  V b | CB (Callback d b)

data D3Selection d = DocumentSelect Selector
                   | SubSelect (D3Selection d) Selector
                   | Merge     (D3Selection d) (D3Selection d)
                   | Data      (D3Selection d) d
                   | Remove    (D3Selection d)
                   | Append    (D3ElementType d)
                   | Enter     (D3Selection d)
                   | Exit      (D3Selection d)

infixr 5 Data as <->

data D3ElementType d =  Circle (Attrs d)
                      | Rect   (Attrs d)
                      | Path   (Attrs d)
                      | Image  (Attrs d)
                      | Text   (Attrs d)
                      | Group  (Array (D3ElementType d))

type SVGPathString = String -- could validate this here at least with runtime constructor
type Attrs d = Array (Attr d)
data Attr d  = CX                  (ValueOrCallback d Number)  -- circles only
             | CY                  (ValueOrCallback d Number)  -- circles only
             | R                   (ValueOrCallback d Number)  -- circles only
             | Fill                (ValueOrCallback d Color)
             | X                   (ValueOrCallback d Number)
             | Y                   (ValueOrCallback d Number)
             | Id                  (ValueOrCallback d String)
             | Height              (ValueOrCallback d Number)
             | Width               (ValueOrCallback d Number)
             | Stroke              (ValueOrCallback d Color)
             | D                   (ValueOrCallback d SVGPathString)
             | StrokeWidth         (ValueOrCallback d Number)
             | StrokeOpacity       (ValueOrCallback d Number)
             | FillOpacity         (ValueOrCallback d Number)
             | StrokeLineCap       (ValueOrCallback d String) -- Round | Butt | Square
             | PatternUnits        (ValueOrCallback d String) -- userSpaceOnUse | objectBoundingBox
             | Opacity             (ValueOrCallback d Number)
             | Style        String (ValueOrCallback d String)
             | Class               (ValueOrCallback d String)
             | Type                (ValueOrCallback d String) -- images only
             | EventHandlerS Event (Callback d String) -- click | mouseenter | mouseleave | mouseover
             | EventHandlerN Event (Callback d Number)

-- we're going to validate the Attr against the ElementType they're being
-- invoked on - so that you can't put a radius on text etc. This will be runtime
-- exception, unfortunately but it will be deterministic at least, seems like
-- maybe GADT's are what's needed for this? not sure.

instance showD3Selection :: Show d => Show (D3Selection d) where
  show (DocumentSelect s)          = "DocumentSelect " <> " \"" <> s <> "\""
  show (SubSelect selection s)     = "SubSelect (" <> show selection <> ") \"" <> s <> "\""
  show (Merge selection selection2) = "Merge: \n\t" <> show selection <> "\n\t" <> show selection2
  show (Data selection d)          = "Data " <> show d <> " bound to " <> show selection
  show (Remove selection)          = "Remove " <> show selection
  show (Append _)                  = "Append"
  show (Enter _)                   = "Enter"
  show (Exit _)                    = "Exit"
