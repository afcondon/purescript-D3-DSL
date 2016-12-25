module D3DSL.Base where

import Color (Color)
import DOM.Event.Types (Event)
import Data.List
import Control.Monad.Eff (Eff)
import Prelude (class Ord, class Show, show, (<>), Unit)
import Data.Foldable (class Foldable)
import DOM (DOM)

type Selector = String

newtype Hierarchical d =
    Hierarchical { name     :: String
                 , children :: Array (Hierarchical d)
                 , datum    :: d
                 }
foreign import data D3         :: ! -- the Effect of D3
foreign import data Peers      :: * -- a D3Selection that's passed back in some callbacks
foreign import data DomElement :: * -- the `this` pointer in a callback, DOM element receiving an event

-- foreign import runD3Fn :: ∀ d i eff. D3S d i -> Eff (d3::D3, dom::DOM|eff) Unit
foreign import runD3Fn :: ∀ d i eff. String -> Eff (d3::D3, dom::DOM|eff) Unit

foreign import getAttrN :: ∀ d i. D3Selection d i -> String -> Number
foreign import getAttrS :: ∀ d i. D3Selection d i -> String -> String

foreign import invisible :: String
foreign import opaque    :: String

type D3S d i = List (D3Selection d i)

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

runD3 :: ∀ d i eff. D3Selection d i -> Eff (d3::D3, dom::DOM|eff) Unit
runD3 (DocumentSelect _)    = runD3Fn "DocumentSelect"
runD3 (DocumentSelectAll _) = runD3Fn "DocumentSelectAll"
runD3 (Select _)            = runD3Fn "Select"
runD3 (SelectAll _)         = runD3Fn "SelectAll"
runD3 (Merge _)             = runD3Fn "Merge"
runD3 (Append _)            = runD3Fn "Append"
runD3 Remove                = runD3Fn "Remove"
runD3 Enter                 = runD3Fn "Enter"
runD3 Exit                  = runD3Fn "Exit"
runD3 (Transition _)        = runD3Fn "Transition"
runD3 (Attrs _)             = runD3Fn "Attrs"
runD3 (DataA _)             = runD3Fn "DataA"
runD3 (DataH _)             = runD3Fn "DataH"
runD3 (DataAI _ _)          = runD3Fn "DataAI"
runD3 (DataHI _ _)          = runD3Fn "DataHI"

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
   | Select            Selector
   | SelectAll         Selector
   | Merge     (D3Selection d i)
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
  show (Select select)       = "Select \""    <> select <> "\""
  show (SelectAll select)    = "SelectAll \"" <> select <> "\""
  show (Merge s)             = "Merge: \n\t"  <> show s <> "\n\t"
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
