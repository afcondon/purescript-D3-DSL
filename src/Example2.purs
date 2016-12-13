module D3DSL.Example2 where

import Prelude (($))
import Data.Array ((..))
import D3DSL

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

gid :: D3Selection (Array Int)
gid = Remove $ Merge (bel <-> 1..10) (bel <-> 5..12)
