module D3DSL.Eval where

import D3DSL.Foreign.Run
import Control.Monad.Eff (Eff)
import D3DSL.Base (D3, D3Action(..), D3DocSelect(..), D3S, PossibleSelection, D3Err(..))
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Function.Eff (runEffFn1, runEffFn2)
import Data.List (List, fromFoldable)
import Prelude (pure, ($), (<$>), bind)

type D3Effects e v = Eff (d3::D3,dom::DOM|e) v

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

d3InitEval :: ∀ d i e. D3DocSelect d i -> D3Effects e PossibleSelection
d3InitEval docSelect = runD3InitAction docSelect

d3EvalActions :: ∀ d i e. PossibleSelection -> D3S d i -> D3Effects e PossibleSelection
d3EvalActions (Right selection) actions = pure $ Right selection
d3EvalActions left _ = pure $ left -- propagate the error

runD3InitAction :: ∀ d i e. D3DocSelect d i -> Eff (d3::D3, dom::DOM|e) PossibleSelection
runD3InitAction (DocumentSelect selector)    = Right <$> runEffFn1 d3DocSelectFn selector
runD3InitAction (DocumentSelectAll selector) = Right <$> runEffFn1 d3DocSelectAllFn selector

runD3Action :: ∀ d i e. D3Action d i -> PossibleSelection -> Eff (d3::D3, dom::DOM|e) PossibleSelection
runD3Action (Select selector) (Right selection)
    = Right <$> runEffFn2 d3SelectFn selector selection

runD3Action (SelectAll selector) (Right selection)
    = Right <$> runEffFn2 d3SelectAllFn selector selection

runD3Action (Merge (Right original)) (Right merged)
    = Right <$> runEffFn2 d3MergeFn merged original

runD3Action Remove (Right selection)
    = do
        runEffFn1 d3RemoveFn selection
        pure $ Left SelectionRemoved

runD3Action Enter (Right selection)
    = Right <$> runEffFn1 d3EnterFn selection

runD3Action Exit (Right selection)
    = Right <$> runEffFn1 d3ExitFn selection

-- runD3Action (Append _)
--     = dummyD3Fn "Append"

runD3Action _ _ = pure $ Left $ JSerr "unhandled action in runD3Action"
{-
runD3Action (Transition _)
    = dummyD3Fn "Transition"

runD3Action (Attrs _)
    = dummyD3Fn "Attrs"

runD3Action (DataA d)
    = runEffFn2 d3DataAFn d emptySelection

runD3Action (DataH _)
    = dummyD3Fn "DataH"

runD3Action (DataAI _ _)
    = dummyD3Fn "DataAI"

runD3Action (DataHI _ _)
    = dummyD3Fn "DataHI"

-}
