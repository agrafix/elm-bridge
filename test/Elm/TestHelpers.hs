module Elm.TestHelpers where

import Elm.Derive

fieldDropOpts :: Int -> DeriveOpts
fieldDropOpts i =
    defaultOpts
    { do_fieldModifier = drop i
    }
