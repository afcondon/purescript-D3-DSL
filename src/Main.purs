module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.Event.Types (Event, TouchEvent)
import DOM.HTML.Types (HTMLElement)
import Data.Array ((..))
import Data.Char (toCharCode)
import Data.Generic (gShow, class Generic)
import Data.Int (toNumber)

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
                   | Append    (D3ElementRecipe d)

data D3ElementType d =  Circle
                      | Rect
                      | Path
                      | Image
                      | Group (Array (D3ElementRecipe d))
                      | Text

data D3ElementRecipe d = D3Recipe (D3ElementType d) (Array (Attr d))
--
data Attr d  = CX (ValueOrCallback d Number)
             | CY (ValueOrCallback d Number)
             | R  (ValueOrCallback d Number)
             | Style String (ValueOrCallback d String)
             | EventHandlerS Event (Callback d String)
             | EventHandlerN Event (Callback d Number)

-- we're going to validate the Attr against the ElementType they're being
-- invoked on - this will be runtime exception, unfortunately but it will be
-- deterministic at least, seems like maybe GADT's are what's needed for this?
-- not sure.

type MyData = { x :: Number, y :: Number, radius :: Number, something :: Number }

-- | examples
hoy :: ValueOrCallback MyData Number
hoy = V 5.0

ist :: ValueOrCallback Char Number
ist = CB $ Lambda \d -> toNumber $ toCharCode d

jud = D3Recipe Circle []

kef = D3Recipe Circle [CX hoy
                     , CY (CB $ Lambda \d -> d.y)
                     , R  (CB $ DIfn \d i -> d.something)]

-- exampleRecipe = D3Recipe Circle [ CX hoy, CY (CB (Lambda \d -> d.y)), R (CB (DIfn \d i -> d.something)) ]


-- | end of examples

instance showD3Selection :: Show d => Show (D3Selection d) where
  show (DocumentSelect s)          = "DocumentSelect " <> " \"" <> s <> "\""
  show (SubSelect selection s)     = "SubSelect (" <> show selection <> ") \"" <> s <> "\""
  show (Merge selection selection2) = "Merge: \n\t" <> show selection <> "\n\t" <> show selection2
  show (Data selection d)          = "Data " <> show d <> " bound to " <> show selection
  show (Remove selection)          = "Remove " <> show selection
  show (Append _)                  = "Append"

-- | if you could define a Semigroup for Selections then you could <> them
-- | but would it make sense for all the constructors above?
-- | could be that the constructors should be chosen so that you can have associativity

d3Select :: ∀ a. String -> D3Selection a
d3Select selector = DocumentSelect selector

d3SelectAll :: ∀ a. String -> D3Selection a
d3SelectAll selector = DocumentSelect selector

d3SubSelect :: ∀ a. D3Selection a -> String -> D3Selection a
d3SubSelect selection selector = SubSelect selection selector

d3SubSelectAll :: ∀ a. D3Selection a -> String -> D3Selection a
d3SubSelectAll selection selector = SubSelect selection selector

bindData :: ∀ a. D3Selection a -> a -> D3Selection a
bindData s a = Data s a

infixr 5 bindData as <->

awn :: forall t7. D3Selection t7
awn = DocumentSelect ".svg"

bel :: forall t9. D3Selection t9
bel = SubSelect awn "circle"

cep :: D3Selection (Array Int)
cep = bel <-> 1..10

dof :: D3Selection (Array Int)
dof = bel <-> 5..20

erg :: D3Selection (Array Int)
erg = Merge dof cep

fub :: D3Selection (Array Int)
fub = Remove erg

gid = Remove $ Merge (bel <-> 1..10) (bel <-> 5..12)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- log $ show fub
  log "hello sailor"
