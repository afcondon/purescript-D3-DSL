module D3DSL.Run where

import Data.List (List, fromFoldable)
import D3DSL.Base (D3, D3Selection, Selection(..))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Foldable (class Foldable)

type ReturnedSelection e =  Eff (d3::D3, dom::DOM|e) D3Selection

foreign import dummyD3Fn        :: ∀ e. String                     -> ReturnedSelection e

foreign import d3SelectFn       :: ∀ e. String                     -> ReturnedSelection e

foreign import d3SelectAllFn    :: ∀ e. String      -> D3Selection -> ReturnedSelection e

foreign import d3DataFn         :: ∀ d e. (Array d) -> D3Selection -> ReturnedSelection e

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

-- we need to add the bound D3Selection into the sig at some point, but
-- hopefully in some way hidden from the DSL

-- runD3 :: ∀ d i e. Selection d i -> D3Selection -> Eff (d3::D3, dom::DOM|e) D3Selection
runD3 :: ∀ d i e. Selection d i -> Eff (d3::D3, dom::DOM|e) D3Selection
runD3 (DocumentSelect selector) = d3SelectFn selector

runD3 (DocumentSelectAll _) = dummyD3Fn "DocumentSelectAll"

runD3 (Select _)            = dummyD3Fn "Select"

runD3 (SelectAll _)         = dummyD3Fn "SelectAll"

runD3 (Merge _)             = dummyD3Fn "Merge"

runD3 (Append _)            = dummyD3Fn "Append"

runD3 Remove                = dummyD3Fn "Remove"

runD3 Enter                 = dummyD3Fn "Enter"

runD3 Exit                  = dummyD3Fn "Exit"

runD3 (Transition _)        = dummyD3Fn "Transition"

runD3 (Attrs _)             = dummyD3Fn "Attrs"

runD3 (DataA _)             = dummyD3Fn "DataA"

runD3 (DataH _)             = dummyD3Fn "DataH"

runD3 (DataAI _ _)          = dummyD3Fn "DataAI"

runD3 (DataHI _ _)          = dummyD3Fn "DataHI"
