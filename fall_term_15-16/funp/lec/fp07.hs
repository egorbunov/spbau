module Fp07 where
import Data.Monoid
import qualified Data.Foldable as F

-- *Fp07> Sum 3 `mappend` Sum 2
-- Sum {getSum = 5}
-- *Fp07> Product 3 `mappend` Product 2
-- Product {getProduct = 6}

