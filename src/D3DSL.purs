module D3DSL where

import Color (Color)
import DOM.Event.Types (Event)
import Prelude (class Ord, class Show, show, (<>))

type Selector = String

newtype Hierarchical d =
    Hierarchical { name     :: String
                 , children :: Array (Hierarchical d)
                 , datum    :: d
                 }
foreign import data D3         :: ! -- the Effect of D3
foreign import data Peers      :: * -- a D3Selection that's passed back in some callbacks
foreign import data DomElement :: * -- the `this` pointer in a callback, DOM element receiving an event

foreign import getAttrN :: ∀ d i. D3Selection d i -> String -> Number
foreign import getAttrS :: ∀ d i. D3Selection d i -> String -> String

foreign import invisible :: String
foreign import opaque    :: String

data Duration = Seconds Int | MS Int

data D3Transition = SimpleTransition Duration
                  | NamedTransition String Duration

data Callback d b =   Lambda1 (d ->                                  b)
                    | Lambda2 (d -> Number ->                        b)
                    | Lambda4 (d -> Number -> Peers -> DomElement -> b)

data ValueOrCallback d b =  V b
                          | F (Callback d b)

data D3Selection d i =
     DocumentSelect    Selector
   | DocumentSelectAll Selector
   | Select            Selector          (D3Selection d i)
   | SelectAll         Selector          (D3Selection d i)
   | Merge     (D3Selection d i)         (D3Selection d i)
   | Append    D3ElementType             (D3Selection d i)
   | Remove                              (D3Selection d i)
   | Enter                               (D3Selection d i)
   | Exit                                (D3Selection d i)
   | Transition D3Transition             (D3Selection d i)
   | Attrs     (Array (Attr d))          (D3Selection d i)
   | DataA     (Array d)                 (D3Selection d i)
   | DataH     (Hierarchical d)          (D3Selection d i)
   | DataAI    (Array d)        (d -> i) (D3Selection d i)
   | DataHI    (Hierarchical d) (d -> i) (D3Selection d i)

data D3ElementType = SvgCircle | SvgRect | SvgPath | SvgImage | SvgText | SvgGroup

type SVGPathString = String -- could validate this here at least with runtime constructor
data Attr d  = CX                  (ValueOrCallback d Number)  -- circles only
             | CY                  (ValueOrCallback d Number)  -- circles only
             | R                   (ValueOrCallback d Number)  -- circles only
             | X                   (ValueOrCallback d Number)
             | Y                   (ValueOrCallback d Number)
             | DX                  (ValueOrCallback d String) -- string because it might have units with
             | DY                  (ValueOrCallback d String)
             | Height              (ValueOrCallback d Number)
             | Width               (ValueOrCallback d Number)
             | StrokeWidth         (ValueOrCallback d Number)
             | StrokeOpacity       (ValueOrCallback d Number)
             | FillOpacity         (ValueOrCallback d Number)
             | Opacity             (ValueOrCallback d Number)
             | D                   (ValueOrCallback d SVGPathString)
             | Id                  (ValueOrCallback d String)
             | StrokeLineCap       (ValueOrCallback d String) -- Round | Butt | Square
             | PatternUnits        (ValueOrCallback d String) -- userSpaceOnUse | objectBoundingBox
             | Style        String (ValueOrCallback d String)
             | Class               (ValueOrCallback d String)
             | Text                (ValueOrCallback d String)
             | Type                (ValueOrCallback d String) -- images only
             | Fill                (ValueOrCallback d Color)
             | Stroke              (ValueOrCallback d Color)
             | Transform    String
             | EventHandlerS Event (Callback d String) -- click | mouseenter | mouseleave | mouseover
             | EventHandlerN Event (Callback d Number)

-- we're going to validate the Attr against the ElementType they're being
-- invoked on - so that you can't put a radius on text etc. This will be runtime
-- exception, unfortunately but it will be deterministic at least, seems like
-- maybe GADT's are what's needed for this? not sure.

instance showD3Selection :: Show d => Show (D3Selection d i) where
  show (DocumentSelect s)    = "DocumentSelect "    <> " \"" <> s <> "\""
  show (DocumentSelectAll s) = "DocumentSelectAll " <> " \"" <> s <> "\""
  show (Select select s)    = "Select \""    <> select <> "\" in " <> show s
  show (SelectAll select s) = "SelectAll \"" <> select <> "\" in " <> show s
  show (Merge s1 s2)        = "Merge: \n\t"  <> show s1
                                  <> "\n\t"  <> show s2
  show (Remove s)           = "Remove "      <> show s
  show (Append element s)   = "Append "      <> show element <> " to " <> show s
  show (Enter s)            = "Enter "       <> show s
  show (Exit s)             = "Exit "        <> show s
  show (Transition t s)     = "Transition "  <> show s
  show (Attrs attrs s)      = "Attrs: "      <> show attrs
  show (DataA d s)          = "Data " <> show d <> " bound to " <> show s
  show (DataAI d _ s)       = "Data " <> show d <> " bound to " <> show s
  show (DataH  d s)         = "Data " <> show d <> " bound to " <> show s
  show (DataHI d _ s)       = "Data " <> show d <> " bound to " <> show s

instance showD3ElementType :: Show D3ElementType where
  show SvgCircle = "Circle"
  show SvgRect   = "Rect"
  show SvgPath   = "Path"
  show SvgImage  = "Image"
  show SvgText   = "Text"
  show SvgGroup  = "Group"

instance showAttr :: Show (Attr d) where
  show (CX a)            = "CX " <> show a
  show (CY a)            = "CY " <> show a
  show (R a)             = "R " <> show a
  show (Fill a)          = "Fill " <> show a
  show (X a)             = "X " <> show a
  show (Y a)             = "Y " <> show a
  show (DX a)            = "DX " <> show a
  show (DY a)            = "DY " <> show a
  show (Id a)            = "Id " <> show a
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
  show (Text a)          = "Text " <> show a
  show (Type a)          = "Type " <> show a
  show (Style style attr) = "Style " <> style <> " " <> show attr
  show (Transform translate) = "Transform(" <> translate <> ")"
  show (EventHandlerS ev _) = "EventHandlerS for " <> eventToString ev
  show (EventHandlerN ev _) = "EventHandlerN for " <> eventToString ev

instance showValueOrCallback :: Show (ValueOrCallback d b) where
  show (V _)  = "Value"
  show (F _) = "Lambda"

instance showHierarchical :: Show (Hierarchical d) where
  show (Hierarchical { name: name }) = "Hierarchical data with root named: " <> show name

eventToString :: Event -> String
eventToString ev = "JS event"
