module Elm.TestHelpers where

import Elm.Derive

fieldDropOpts :: Int -> Options
fieldDropOpts i =
    defaultOptions
    { fieldLabelModifier = drop i
    }
