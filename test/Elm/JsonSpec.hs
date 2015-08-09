{-# LANGUAGE TemplateHaskell #-}
module Elm.JsonSpec (spec) where

import Elm.Derive
import Elm.TyRep
import Elm.Json

import Elm.TestHelpers

import Data.Proxy
import Test.Hspec

data Foo
   = Foo
   { f_name :: String
   , f_blablub :: Int
   } deriving (Show, Eq)

data Bar a
   = Bar
   { b_name :: a
   , b_blablub :: Int
   , b_tuple :: (Int, String)
   , b_list :: [Bool]
   } deriving (Show, Eq)

data SomeOpts a
   = Okay Int
   | NotOkay a

$(deriveElmDef (fieldDropOpts 2) ''Foo)
$(deriveElmDef (fieldDropOpts 2) ''Bar)
$(deriveElmDef defaultOpts ''SomeOpts)

fooSer :: String
fooSer = "jsonEncFoo  val = \n   Json.Encode.object\n   [ (\"name\", Json.Encode.string val.name)\n   , (\"blablub\", Json.Encode.int val.blablub)\n   ]\n"

fooParse :: String
fooParse = "jsonDecFoo  = \n   (\"name\" := Json.Decode.string) `Json.Decode.andThen` \\pname -> \n   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub -> \n   Json.Decode.succeed {name = pname, blablub = pblablub}\n"

barSer :: String
barSer = "jsonEncBar localEncoder_a val = \n   Json.Encode.object\n   [ (\"name\", localEncoder_a val.name)\n   , (\"blablub\", Json.Encode.int val.blablub)\n   , (\"tuple\", (\\v1 v2 -> [(Json.Encode.int) v1,(Json.Encode.string) v2] val.tuple)\n   , (\"list\", (Json.Encode.list << map Json.Encode.bool) val.list)\n   ]\n"

barParse :: String
barParse = "jsonDecBar localDecoder_a = \n   (\"name\" := localDecoder_a) `Json.Decode.andThen` \\pname -> \n   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub -> \n   (\"tuple\" := Json.Decode.tuple2 (,) (Json.Decode.int) (Json.Decode.string)) `Json.Decode.andThen` \\ptuple -> \n   (\"list\" := Json.Decode.list (Json.Decode.bool)) `Json.Decode.andThen` \\plist -> \n   Json.Decode.succeed {name = pname, blablub = pblablub, tuple = ptuple, list = plist}\n"

someOptsParse :: String
someOptsParse = "jsonDecSomeOpts localDecoder_a = \n   Json.Decode.oneOf \n   [ (\"Okay\" := Json.tuple1 Okay (Json.Decode.int))\n   , (\"NotOkay\" := Json.tuple1 NotOkay (localDecoder_a))\n   ]\n"

someOptsSer :: String
someOptsSer = "jsonEncSomeOpts localEncoder_a val = \n   case val of\n      Okay v1 -> [Json.Encode.int v1]\n      NotOkay v1 -> [localEncoder_a v1]\n"

spec :: Spec
spec =
    describe "json serialisation" $
    do let rFoo = compileElmDef (Proxy :: Proxy Foo)
           rBar = compileElmDef (Proxy :: Proxy (Bar a))
           rSomeOpts = compileElmDef (Proxy :: Proxy (SomeOpts a))
       it "should produce the correct ser code" $
          do jsonSerForDef rFoo `shouldBe` fooSer
             jsonSerForDef rBar `shouldBe` barSer
             jsonSerForDef rSomeOpts `shouldBe` someOptsSer
       it "should produce the correct parse code" $
          do jsonParserForDef rFoo `shouldBe` fooParse
             jsonParserForDef rBar `shouldBe` barParse
             jsonParserForDef rSomeOpts `shouldBe` someOptsParse
