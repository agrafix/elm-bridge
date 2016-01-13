{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
import "elm-bridge" Elm.Derive
import "elm-bridge" Elm.Module

import Data.Proxy

data Foo
   = Foo
   { f_name :: String
   , f_blablub :: Int
   } deriving (Show, Eq)

deriveBoth defaultOptions ''Foo

main :: IO ()
main =
    putStrLn $ makeElmModule "Foo"
    [ DefineElm (Proxy :: Proxy Foo)
    ]
