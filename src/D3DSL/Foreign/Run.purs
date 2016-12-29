module D3DSL.Foreign.Run where

import D3DSL.Base (Attr, D3, D3ElementType, D3Selection, D3Transition, Hierarchical)
import DOM (DOM)
import Data.Function.Eff (EffFn1, EffFn2)
import Prelude (Unit)

-- | All the foreign effectful functions
foreign import d3DocSelectFn ::
	∀ e. EffFn1 (d3::D3, dom::DOM|e) String D3Selection
foreign import d3DocSelectAllFn ::
	∀ e. EffFn1 (d3::D3, dom::DOM|e) String D3Selection
foreign import d3SelectFn ::
	∀ e. EffFn2 (d3::D3, dom::DOM|e) String D3Selection D3Selection
foreign import d3SelectAllFn ::
	∀ e. EffFn2 (d3::D3, dom::DOM|e) String D3Selection D3Selection
foreign import d3MergeFn ::
	∀ e. EffFn2 (d3::D3, dom::DOM|e) D3Selection D3Selection D3Selection
foreign import d3AppendFn ::                -- D3ElementType needs conversion from ADT
	∀ e. EffFn2 (d3::D3, dom::DOM|e) D3ElementType D3Selection D3Selection
foreign import d3RemoveFn ::
	∀ e. EffFn1 (d3::D3, dom::DOM|e) D3Selection Unit -- no result from a Remove call
foreign import d3EnterFn ::
	∀ e. EffFn1 (d3::D3, dom::DOM|e) D3Selection D3Selection
foreign import d3ExitFn ::
	∀ e. EffFn1 (d3::D3, dom::DOM|e) D3Selection D3Selection
foreign import d3TransitionFn ::            -- D3Transition needs conversion from ADT
	∀ e. EffFn2 (d3::D3, dom::DOM|e) D3Transition D3Selection D3Selection
foreign import d3AttrsFn ::                 -- Attrs need conversion from ADT
	∀ d e. EffFn2 (d3::D3, dom::DOM|e) ( Array (Attr d) ) D3Selection D3Selection
foreign import d3DataAFn ::
	∀ d e. EffFn2 (d3::D3, dom::DOM|e) ( Array d) D3Selection D3Selection
foreign import d3DataHFn ::
	∀ d e. EffFn2 (d3::D3, dom::DOM|e) ( Hierarchical d) D3Selection D3Selection
