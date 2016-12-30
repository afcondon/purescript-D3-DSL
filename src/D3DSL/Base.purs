module D3DSL.Base where

import Data.List
import Color (Color)
import DOM.Event.Types (Event)
import Data.Either (Either)
import Prelude (class Show, show, (<>))

type Selector = String

newtype Hierarchical d =
    Hierarchical { name     :: String
                 , children :: Array (Hierarchical d)
                 , datum    :: d
                 }


-- ||               Core foreign imports
-- the Effect of D3
foreign import data D3          :: !
-- the underlying D3 selection that is passed between calls
foreign import data D3Selection :: *
-- a Selection that's passed back in some callbacks
foreign import data Peers       :: *
-- the `this` pointer in a callback, DOM element receiving an event
foreign import data DomElement  :: *

type PossibleSelection = Either D3Err D3Selection
data D3Err             = Uninitialized
                       | EmptyActionList
                       | JSerr String
                       | SelectionRemoved

foreign import getAttrN :: ∀ d i. D3Action d i -> String -> Number
foreign import getAttrS :: ∀ d i. D3Action d i -> String -> String

foreign import invisible :: String
foreign import opaque    :: String

type D3S d i = List (D3Action d i)

data Duration = Seconds Int | MS Int

data D3Transition = SimpleTransition Duration
                  | NamedTransition String Duration

data Callback d b =   Lambda1 (d ->                                  b)
                    | Lambda2 (d -> Number ->                        b)
                    | Lambda4 (d -> Number -> Peers -> DomElement -> b)

data ValueOrCallback d b =  V b
                          | F (Callback d b)


data D3DocSelect d i =
     DocumentSelect    Selector
   | DocumentSelectAll Selector

data D3Action d i =
     Select            Selector
   | SelectAll         Selector
   | Merge     PossibleSelection
   | Insert    D3ElementType
   | Append    D3ElementType
   | Remove
   | Enter
   | Exit
   | Transition D3Transition
   | Attrs     (Array (Attr d))
   | DataA     (Array d)
   | DataH     (Hierarchical d)
   | DataAI    (Array d)        (d -> i)
   | DataHI    (Hierarchical d) (d -> i)

-- ADT for the SVG primitives (which are relatively few). At present no support
-- for general (ie untyped) elements, let's just see if it's necessary. It will
-- be necessary to add types for (almost?) ALL of the HTML elements if there is
-- no untyped form so maybe get the element types for HTML from another lib or
-- go with untyped option too or just make it all untyped

data D3ElementType
    =     SvgCircle
        -- | SvgEllipse
        | SvgGroup
        | SvgImage
        -- | SvgLine
        -- | SvgMesh
        | SvgPath
        -- | SvgPolygon
        -- | SvgPolyline
        | SvgRect
        | SvgText
        -- | SvgUse

-- as with the element types above there are a LOT of SVG attribute types:
-- alignment-baseline, baseline-shift, clip, clip-path, clip-rule, color,
-- color-interpolation, color-interpolation-filters, color-profile,
-- color-rendering, cursor, direction, display, dominant-baseline,
-- enable-background, fill, fill-opacity, fill-rule, filter, flood-color,
-- flood-opacity, font-family, font-size, font-size-adjust, font-stretch,
-- font-style, font-variant, font-weight, glyph-orientation-horizontal,
-- glyph-orientation-vertical, image-rendering, kerning, letter-spacing,
-- lighting-color, marker-end, marker-mid, marker-start, mask, opacity,
-- overflow, pointer-events, shape-rendering, stop-color, stop-opacity, stroke,
-- stroke-dasharray, stroke-dashoffset, stroke-linecap, stroke-linejoin,
-- stroke-miterlimit, stroke-opacity, stroke-width, text-anchor,
-- text-decoration, text-rendering, unicode-bidi, visibility, word-spacing,
-- writing-mode

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
             | Transform    String  -- would be nice to build this up from ADT too
             | EventHandlerS Event (Callback d String) -- click | mouseenter | mouseleave | mouseover
             | EventHandlerN Event (Callback d Number)

-- we're going to validate the Attr against the ElementType they're being
-- invoked on - so that you can't put a radius on text etc. This will be runtime
-- exception, unfortunately but it will be deterministic at least, seems like
-- maybe GADT's are what's needed for this? not sure.
instance showD3DocSelect :: Show d => Show (D3DocSelect d i) where
  show (DocumentSelect s)    = "DocumentSelect "    <> " \"" <> s <> "\""
  show (DocumentSelectAll s) = "DocumentSelectAll " <> " \"" <> s <> "\""

instance showD3Action :: Show d => Show (D3Action d i) where
  show (Select select)       = "Select \""    <> select <> "\""
  show (SelectAll select)    = "SelectAll \"" <> select <> "\""
  show (Merge _)             = "Merge: <D3 Selection can't be shown>"
  show (Insert element)      = "Insert " <> show element <>  " to "
  show (Append element)      = "Append " <> show element <>  " to "
  show (Remove)              = "Remove"
  show (Enter)               = "Enter"
  show (Exit)                = "Exit"
  show (Transition t)        = "Transition"
  show (Attrs attrs)         = "Attrs: "      <> show attrs
  show (DataA d)             = "Data " <> show d
  show (DataAI d _)          = "Data " <> show d
  show (DataH  d)            = "Data " <> show d
  show (DataHI d _)          = "Data " <> show d

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
  show (Style style attr)    = "Style " <> style <> " " <> show attr
  show (Transform translate) = "Transform(" <> translate <> ")"
  show (EventHandlerS ev _)  = "EventHandlerS for " <> eventToString ev
  show (EventHandlerN ev _)  = "EventHandlerN for " <> eventToString ev

instance showValueOrCallback :: Show (ValueOrCallback d b) where
  show (V _)  = "Value"
  show (F _) = "Lambda"

instance showHierarchical :: Show (Hierarchical d) where
  show (Hierarchical { name: name }) = "Hierarchical data with root named: " <> show name

eventToString :: Event -> String
eventToString ev = "JS event"
