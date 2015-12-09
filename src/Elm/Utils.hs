module Elm.Utils where

import Data.Char (toUpper)

cap :: String -> String
cap "" = ""
cap (x:xs) = toUpper x : xs

fixReserved :: String -> String
fixReserved x = case x of
                    "in"   -> "in_"
                    "type" -> "type_"
                    _      -> x
