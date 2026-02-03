module DataFrame (
    module DataFrame.Core,
    module DataFrame.Column,
    module DataFrame.Expression,
    module DataFrame.Functions,
    module DataFrame.Permutation,
    module DataFrame.PrettyPrint,
    (|>),
) where

import Data.Function

import DataFrame.Column
import DataFrame.Core
import DataFrame.Expression
import DataFrame.Functions
import DataFrame.Permutation (sortBy, SortOrder(..))
import DataFrame.PrettyPrint

(|>) :: a -> (a -> b) -> b
(|>) = (&)
