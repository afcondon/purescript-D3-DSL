module D3DSL.Eval where

import D3DSL.Foreign.Run
import Control.Monad.Eff (Eff)
import D3DSL.Base (D3, D3Action(..), D3S)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Function.Eff (runEffFn1, runEffFn2)
import Data.List (List, fromFoldable, head, (:))
import Data.Maybe (Maybe(..))
import Prelude (($), (<$>), pure, Unit, unit, bind)

d3s :: forall f a. (Foldable f) => f a -> List a
d3s = fromFoldable

runD3Action :: ∀ d i e. D3Action d i -> PossibleSelection -> Eff (d3::D3, dom::DOM|e) PossibleSelection
runD3Action (DocumentSelect selector) _ -- don't care about PossibleSelection if making new docSelect
    = Right <$> runEffFn1 d3DocSelectFn selector

runD3Action (DocumentSelectAll selector) _ -- don't care about PossibleSelection if making new docSelect
    = Right <$> runEffFn1 d3DocSelectAllFn selector

runD3Action (Select selector) (Right selection)
    = Right <$> runEffFn2 d3SelectFn selector selection

runD3Action (SelectAll selector) (Right selection)
    = Right <$> runEffFn2 d3SelectAllFn selector selection

runD3Action _ _ = pure $ Left $ JSerr "unhandled action in runD3Action"
{-
runD3Action (Merge _)
    = dummyD3Fn "Merge"

runD3Action (Append _)
    = dummyD3Fn "Append"

runD3Action Remove
    = dummyD3Fn "Remove"

runD3Action Enter
    = dummyD3Fn "Enter"

runD3Action Exit
    = dummyD3Fn "Exit"

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
type D3Effects e v = Eff (d3::D3,dom::DOM|e) v

-- headAsEither :: ∀ d err. err -> List d -> Either err d
-- headAsEither err l = case head l of
--                      Nothing -> Left err
--                      Just el -> Right el

evalD3 :: ∀ d i e. D3S d i -> D3Effects e Unit
evalD3 (firstAction:rest) = do
    initialSelect <- runD3Action firstAction (Left Uninitialized)
    pure unit
evalD3 _ = pure unit