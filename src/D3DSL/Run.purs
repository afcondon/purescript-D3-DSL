module D3DSL.Run where

import Data.List
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Foldable (class Foldable)
import D3DSL.Base
import Prelude (Unit)

foreign import runD3Fn :: ∀ d i eff. String -> Eff (d3::D3, dom::DOM|eff) Unit

foreign import d3SelectFn :: ∀ d i eff. String -> Eff (d3::D3, dom::DOM|eff) Unit

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

runD3 :: ∀ d i eff. D3Selection d i -> Eff (d3::D3, dom::DOM|eff) Unit
runD3 (DocumentSelect selector)    = d3SelectFn selector

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
