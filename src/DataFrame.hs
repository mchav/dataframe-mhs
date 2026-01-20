module DataFrame (
    module Core,
    module Column,
    module Expression,
    module Functions,
    module PrettyPrint,

    (|>)
) where

import Data.Function

import qualified DataFrame.Core as Core
import qualified DataFrame.Column as Column
import qualified DataFrame.Expression as Expression
import qualified DataFrame.Functions as Functions
import qualified DataFrame.PrettyPrint as PrettyPrint

(|>) :: a -> (a -> b) -> b
(|>) = (&)
