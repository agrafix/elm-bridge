module Elm.Utils where

import           Data.Char (toUpper)

cap :: String -> String
cap ""     = ""
cap (x:xs) = toUpper x : xs

fixReserved :: String -> String
fixReserved x | x `elem` reservedWords = x ++ "_"
              | otherwise = x
    where
        reservedWords = [ "if", "then", "else"
                        , "case", "of"
                        , "let", "in"
                        , "type"
                        , "module", "where"
                        , "import", "as", "hiding", "exposing"
                        , "port", "export", "foreign"
                        , "perform"
                        , "deriving"
                        ]
