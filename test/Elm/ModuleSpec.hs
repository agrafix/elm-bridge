{-# LANGUAGE TemplateHaskell #-}
module Elm.ModuleSpec (spec) where

import Elm.Derive
import Elm.Module

import Elm.TestHelpers

import Data.Proxy
import Test.Hspec

data Bar a
   = Bar
   { b_name :: a
   , b_blablub :: Int
   , b_tuple :: (Int, String)
   , b_list :: [Bool]
   } deriving (Show, Eq)

$(deriveElmDef (fieldDropOpts 2) ''Bar)

moduleCode :: String
moduleCode = "module Foo where \n\nimport Json.Decode\nimport Json.Decode exposing ((:=))\nimport Json.Encode\n\n\ntype alias Bar a = \n   { name: a\n   , blablub: Int\n   , tuple: (Int, String)\n   , list: (List Bool)\n   }\n\njsonDecBar localDecoder_a = \n   (\"name\" := localDecoder_a) `Json.Decode.andThen` \\pname -> \n   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub -> \n   (\"tuple\" := Json.Decode.tuple2 (,) (Json.Decode.int) (Json.Decode.string)) `Json.Decode.andThen` \\ptuple -> \n   (\"list\" := Json.Decode.list (Json.Decode.bool)) `Json.Decode.andThen` \\plist -> \n   Json.Decode.succeed {name = pname, blablub = pblablub, tuple = ptuple, list = plist}\n\njsonEncBar localEncoder_a val = \n   Json.Encode.object\n   [ (\"name\", localEncoder_a val.name)\n   , (\"blablub\", Json.Encode.int val.blablub)\n   , (\"tuple\", (\\v1 v2 -> [(Json.Encode.int) v1,(Json.Encode.string) v2] val.tuple)\n   , (\"list\", (Json.Encode.list << map Json.Encode.bool) val.list)\n   ]\n\n"

spec :: Spec
spec =
    describe "makeElmModule" $
    it "should produce the correct code" $
       do let modu =
                 makeElmModule "Foo" [DefineElm (Proxy :: Proxy (Bar a))]
          modu `shouldBe` moduleCode
