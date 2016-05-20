{-# LANGUAGE TemplateHaskell #-}
module Elm.ModuleSpec (spec) where

import Elm.Derive
import Elm.Module
import Elm.Versions


import Data.Map (Map)
import Data.Proxy
import Test.Hspec

data Bar a
   = Bar
   { b_name :: a
   , b_blablub :: Int
   , b_tuple :: (Int, String)
   , b_list :: [Bool]
   , b_list_map :: [Map String Bool]
   } deriving (Show, Eq)

data Qux a = Qux1 Int String
           | Qux2 { _qux2a :: Int, _qux2test :: a }
           deriving (Show, Eq)

$(deriveElmDef (defaultOptionsDropLower 2) ''Bar)
$(deriveElmDef (defaultOptionsDropLower 5) ''Qux)

moduleHeader' :: ElmVersion -> String -> String
moduleHeader' Elm0p16 name = "module " ++ name ++ " where"
moduleHeader' Elm0p17 name = "module " ++ name ++ " exposing(..)"

moduleCode :: ElmVersion -> String
moduleCode elmVersion = unlines
    [ moduleHeader' elmVersion "Foo"
    , ""
    , "import Json.Decode"
    , "import Json.Decode exposing ((:=))"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import Dict"
    , "import Set"
    , ""
    , ""
    , "type alias Bar a ="
    , "   { name: a"
    , "   , blablub: Int"
    , "   , tuple: (Int, String)"
    , "   , list: (List Bool)"
    , "   , list_map: (List (Dict String Bool))"
    , "   }"
    , ""
    , "jsonDecBar : Json.Decode.Decoder a -> Json.Decode.Decoder ( Bar a )"
    , "jsonDecBar localDecoder_a ="
    , "   (\"name\" := localDecoder_a) `Json.Decode.andThen` \\pname ->"
    , "   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub ->"
    , "   (\"tuple\" := Json.Decode.tuple2 (,) (Json.Decode.int) (Json.Decode.string)) `Json.Decode.andThen` \\ptuple ->"
    , "   (\"list\" := Json.Decode.list (Json.Decode.bool)) `Json.Decode.andThen` \\plist ->"
    , "   (\"list_map\" := Json.Decode.list (Json.Decode.dict (Json.Decode.bool))) `Json.Decode.andThen` \\plist_map ->"
    , "   Json.Decode.succeed {name = pname, blablub = pblablub, tuple = ptuple, list = plist, list_map = plist_map}"
    , ""
    , "jsonEncBar : (a -> Value) -> Bar a -> Value"
    , "jsonEncBar localEncoder_a val ="
    , "   Json.Encode.object"
    , "   [ (\"name\", localEncoder_a val.name)"
    , "   , (\"blablub\", Json.Encode.int val.blablub)"
    , "   , (\"tuple\", (\\(v1,v2) -> Json.Encode.list [(Json.Encode.int) v1,(Json.Encode.string) v2]) val.tuple)"
    , "   , (\"list\", (Json.Encode.list << List.map Json.Encode.bool) val.list)"
    , "   , (\"list_map\", (Json.Encode.list << List.map (encodeMap (Json.Encode.string) (Json.Encode.bool))) val.list_map)"
    , "   ]"
    , ""
    ]

moduleCode' :: ElmVersion -> String
moduleCode' elmVersion = unlines
    [ moduleHeader' elmVersion "Qux"
    , ""
    , "import Json.Decode"
    , "import Json.Decode exposing ((:=))"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import Dict"
    , "import Set"
    , ""
    , ""
    , "type Qux a ="
    , "    Qux1 Int String"
    , "    | Qux2 {a: Int, test: a}"
    , ""
    , "jsonDecQux : Json.Decode.Decoder a -> Json.Decode.Decoder ( Qux a )"
    , "jsonDecQux localDecoder_a ="
    , "    let jsonDecDictQux = Dict.fromList"
    , "            [ (\"Qux1\", Json.Decode.tuple2 Qux1 (Json.Decode.int) (Json.Decode.string))"
    , "            , (\"Qux2\", Json.Decode.map Qux2 (   (\"a\" := Json.Decode.int) `Json.Decode.andThen` \\pa ->    (\"test\" := localDecoder_a) `Json.Decode.andThen` \\ptest ->    Json.Decode.succeed {a = pa, test = ptest}))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"Qux\" jsonDecDictQux"
    , ""
    , "jsonEncQux : (a -> Value) -> Qux a -> Value"
    , "jsonEncQux localEncoder_a val ="
    , "    let keyval v = case v of"
    , "                    Qux1 v1 v2 -> (\"Qux1\", encodeValue (Json.Encode.list [Json.Encode.int v1, Json.Encode.string v2]))"
    , "                    Qux2 vs -> (\"Qux2\", encodeObject [(\"a\", Json.Encode.int vs.a), (\"test\", localEncoder_a vs.test)])"
    , "    in encodeSumObjectWithSingleField keyval val"
    , ""
    ]

spec :: Spec
spec = do
  makeElmModuleSpec
  version0p16Spec
  version0p17Spec

makeElmModuleSpec :: Spec
makeElmModuleSpec =
    describe "makeElmModule" $
    it "should produce the correct code" $
       do let modu = makeElmModule "Foo" [DefineElm (Proxy :: Proxy (Bar a))]
          let modu' = makeElmModule "Qux" [DefineElm (Proxy :: Proxy (Qux a))]
          modu `shouldBe` (moduleCode Elm0p16)
          modu' `shouldBe` (moduleCode' Elm0p16)

version0p16Spec :: Spec
version0p16Spec =
  describe "makeElmModuleWithVersion Elm0p16" $
    it "should produce the correct code" $
       do let modu = makeElmModuleWithVersion Elm0p16 "Foo" [DefineElm (Proxy :: Proxy (Bar a))]
          let modu' = makeElmModuleWithVersion Elm0p16 "Qux" [DefineElm (Proxy :: Proxy (Qux a))]
          modu `shouldBe` (moduleCode Elm0p16)
          modu' `shouldBe` (moduleCode' Elm0p16)

version0p17Spec :: Spec
version0p17Spec =
  describe "makeElmModuleWithVersion Elm0p17" $
    it "should produce the correct code" $
       do let modu = makeElmModuleWithVersion Elm0p17 "Foo" [DefineElm (Proxy :: Proxy (Bar a))]
          let modu' = makeElmModuleWithVersion Elm0p17 "Qux" [DefineElm (Proxy :: Proxy (Qux a))]
          modu `shouldBe` (moduleCode Elm0p17)
          modu' `shouldBe` (moduleCode' Elm0p17)
