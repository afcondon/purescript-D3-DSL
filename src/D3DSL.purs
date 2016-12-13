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
                   | SubSelect  Selector         (D3Selection d)
                   | Merge     (D3Selection d)   (D3Selection d)
                   | Data       d                (D3Selection d)
                   | Append    (D3ElementType d) (D3Selection d)
                   | Remove                      (D3Selection d)
                   | Enter                       (D3Selection d)
                   | Exit                        (D3Selection d)


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
  show (DocumentSelect s)   = "DocumentSelect " <> " \"" <> s <> "\""
  show (SubSelect select s) = "SubSelect \"" <> select <> "\" in " <> show s
  show (Merge s1 s2)        = "Merge: \n\t" <> show s1
                                  <> "\n\t" <> show s2
  show (Data d s)           = "Data " <> show d <> " bound to " <> show s
  show (Remove s)           = "Remove " <> show s
  show (Append element s)   = "Append " <> show element <> " to " <> show s
  show (Enter s)            = "Enter "  <> show s
  show (Exit s)             = "Exit "   <> show s

instance showD3ElementType :: Show d => Show (D3ElementType d) where
  show (Circle attrs) = "Circle " <> show attrs
  show (Rect attrs)   = "Rect " <> show attrs
  show (Path attrs)   = "Path " <> show attrs
  show (Image attrs)  = "Image " <> show attrs
  show (Text attrs)   = "Text " <> show attrs
  show (Group attrs)  = "Group " <> show attrs

instance showAttr :: Show (Attr d) where
  show (CX a)           = "CX " <> show a
  show (CY a)           = "CY " <> show a
  show (R a)            = "R " <> show a
  show (Fill a)         = "Fill " <> show a
  show (X a)            = "X " <> show a
  show (Y a)            = "Y " <> show a
  show (Id a)           = "Id " <> show a
  show (Height a)        = "Height " <> show a
  show (Width a)         = "Width " <> show a
  show (Stroke a)        = "Stroke " <> show a
  show (D a)             = "D " <> show a
  show (StrokeWidth a)   = "StrokeWidth " <> show a
  show (StrokeOpacity a) = "StrokeOpacity " <> show a
  show (FillOpacity a)   = "FillOpacity " <> show a
  show (StrokeLineCap a) = "StrokeLineCap " <> show a
  show (PatternUnits a)  = "PatternUnits " <> show a
  show (Opacity a)       = "Opacity " <> show a
  show (Class a)         = "Class " <> show a
  show (Type a)          = "Type " <> show a
  show (Style style attr) = "Style " <> style <> " " <> show attr
  show (EventHandlerS ev _) = "EventHandlerS for " <> eventToString ev
  show (EventHandlerN ev _) = "EventHandlerN for " <> eventToString ev

instance showValueOrCallback :: Show (ValueOrCallback d b) where
  show (V _)  = "Value"
  show (CB _) = "Lambda"

eventToString :: Event -> String
eventToString ev = "JS event"
