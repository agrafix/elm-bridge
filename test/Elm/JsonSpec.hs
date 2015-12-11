{-# LANGUAGE TemplateHaskell #-}
module Elm.JsonSpec (spec) where

import Elm.Derive
import Elm.TyRep
import Elm.Json

import Data.Proxy
import Test.Hspec
import Data.Char (toLower)
import Data.Aeson.Types (SumEncoding(..))
import qualified Data.Map.Strict as M

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

data UnaryA = UnaryA1 | UnaryA2
data UnaryB = UnaryB1 | UnaryB2

data Change a = Change { _before :: a, _after :: a }

data Baz a = Baz1 { _foo :: Int, _qux :: M.Map Int a }
           | Baz2 { _bar :: Maybe Int, _str :: String }
           | Zob a

data TestComp a = TestComp { _t1 :: Change Int
                           , _t2 :: Change a
                           }

-- TODO
data Qux a = Qux1 { _quxfoo :: Int, _quxqux :: a }
           | Qux2 Int (M.Map Int a)

$(deriveElmDef (defaultOptionsDropLower 2) ''Foo)
$(deriveElmDef (defaultOptionsDropLower 2) ''Bar)
$(deriveElmDef (defaultOptionsDropLower 1) ''TestComp)
$(deriveElmDef defaultOptions ''SomeOpts)
$(deriveElmDef defaultOptions{ allNullaryToStringTag = False } ''UnaryA)
$(deriveElmDef defaultOptions{ allNullaryToStringTag = True  } ''UnaryB)
$(deriveElmDef defaultOptions { fieldLabelModifier = drop 1 . map toLower } ''Baz)
$(deriveElmDef defaultOptions { fieldLabelModifier = drop 4 . map toLower, sumEncoding = TaggedObject "tag" "value" } ''Qux)

fooSer :: String
fooSer = "jsonEncFoo  val =\n   Json.Encode.object\n   [ (\"name\", Json.Encode.string val.name)\n   , (\"blablub\", Json.Encode.int val.blablub)\n   ]\n"

fooParse :: String
fooParse = unlines
    [ "jsonDecFoo : Json.Decode.Decoder ( Foo )"
    , "jsonDecFoo ="
    , "   (\"name\" := Json.Decode.string) `Json.Decode.andThen` \\pname -> "
    , "   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub -> "
    , "   Json.Decode.succeed {name = pname, blablub = pblablub}"
    ]

barSer :: String
barSer = unlines
    [ "jsonEncBar localEncoder_a val ="
    , "   Json.Encode.object"
    , "   [ (\"name\", localEncoder_a val.name)"
    , "   , (\"blablub\", Json.Encode.int val.blablub)"
    , "   , (\"tuple\", (\\v1 v2 -> [(Json.Encode.int) v1,(Json.Encode.string) v2]) val.tuple)"
    , "   , (\"list\", (Json.Encode.list << List.map Json.Encode.bool) val.list)"
    , "   ]"
    ]

bazSer :: String
bazSer = unlines
    [ "jsonEncBaz localEncoder_a val ="
    , "    let keyval v = case v of"
    , "                    Baz1 vs -> (\"Baz1\", EObject [(\"foo\", Json.Encode.int vs.foo), (\"qux\", encodeMap (Json.Encode.int) (localEncoder_a) vs.qux)])"
    , "                    Baz2 vs -> (\"Baz2\", EObject [(\"bar\", (maybe Json.Encode.null (Json.Encode.int)) vs.bar), (\"str\", Json.Encode.string vs.str)])"
    , "                    Zob v1 -> (\"Zob\", EValue (localEncoder_a v1))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

barParse :: String
barParse = unlines
    [ "jsonDecBar : Json.Decode.Decoder a -> Json.Decode.Decoder ( Bar a )"
    , "jsonDecBar localDecoder_a ="
    , "   (\"name\" := localDecoder_a) `Json.Decode.andThen` \\pname -> "
    , "   (\"blablub\" := Json.Decode.int) `Json.Decode.andThen` \\pblablub -> "
    , "   (\"tuple\" := Json.Decode.tuple2 (,) (Json.Decode.int) (Json.Decode.string)) `Json.Decode.andThen` \\ptuple -> "
    , "   (\"list\" := Json.Decode.list (Json.Decode.bool)) `Json.Decode.andThen` \\plist -> "
    , "   Json.Decode.succeed {name = pname, blablub = pblablub, tuple = ptuple, list = plist}"
    ]

bazParse :: String
bazParse = unlines
    [ "jsonDecBaz : Json.Decode.Decoder a -> Json.Decode.Decoder ( Baz a )"
    , "jsonDecBaz localDecoder_a = "
    , "    let jsonDecDictBaz = Dict.fromList"
    , "            [ (\"Baz1\", Json.Decode.map Baz1 (   (\"foo\" := Json.Decode.int) `Json.Decode.andThen` \\pfoo ->     (\"qux\" := decodeMap (Json.Decode.int) (localDecoder_a)) `Json.Decode.andThen` \\pqux ->     Json.Decode.succeed {foo = pfoo, qux = pqux}))"
    , "            , (\"Baz2\", Json.Decode.map Baz2 (   (Json.Decode.maybe (\"bar\" := Json.Decode.int)) `Json.Decode.andThen` \\pbar ->     (\"str\" := Json.Decode.string) `Json.Decode.andThen` \\pstr ->     Json.Decode.succeed {bar = pbar, str = pstr}))"
    , "            , (\"Zob\", Json.Decode.map Zob (localDecoder_a))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"Baz\" jsonDecDictBaz"
    ]

quxParse :: String
quxParse = unlines
    [ "jsonDecQux localDecoder_a = "
    , "   "
    ]

someOptsParse :: String
someOptsParse = unlines
    [ "jsonDecSomeOpts : Json.Decode.Decoder a -> Json.Decode.Decoder ( SomeOpts a )"
    , "jsonDecSomeOpts localDecoder_a = "
    , "    let jsonDecDictSomeOpts = Dict.fromList"
    , "            [ (\"Okay\", Json.Decode.map Okay (Json.Decode.int))"
    , "            , (\"NotOkay\", Json.Decode.map NotOkay (localDecoder_a))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"SomeOpts\" jsonDecDictSomeOpts"
    ]

someOptsSer :: String
someOptsSer = unlines
    [ "jsonEncSomeOpts localEncoder_a val ="
    , "    let keyval v = case v of"
    , "                    Okay v1 -> (\"Okay\", EValue (Json.Encode.int v1))"
    , "                    NotOkay v1 -> (\"NotOkay\", EValue (localEncoder_a v1))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

test1Parse :: String
test1Parse = unlines
    [ "jsonDecTestComp : Json.Decode.Decoder a -> Json.Decode.Decoder ( TestComp a )"
    , "jsonDecTestComp localDecoder_a ="
    , "   (\"t1\" := jsonDecChange (Json.Decode.int)) `Json.Decode.andThen` \\pt1 -> "
    , "   (\"t2\" := jsonDecChange (localDecoder_a)) `Json.Decode.andThen` \\pt2 -> "
    , "   Json.Decode.succeed {t1 = pt1, t2 = pt2}"
    ]

unaryAParse :: String
unaryAParse = unlines
    [ "jsonDecUnaryA : Json.Decode.Decoder ( UnaryA )"
    , "jsonDecUnaryA = "
    , "    let jsonDecDictUnaryA = Dict.fromList"
    , "            [ (\"UnaryA1\", Json.Decode.tuple0 UnaryA1 )"
    , "            , (\"UnaryA2\", Json.Decode.tuple0 UnaryA2 )"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"UnaryA\" jsonDecDictUnaryA"
    ]

unaryBParse :: String
unaryBParse = unlines
    [ "jsonDecUnaryB : Json.Decode.Decoder ( UnaryB )"
    , "jsonDecUnaryB = decodeSumUnaries \"UnaryB\" jsonDecDictUnaryB"
    , "jsonDecDictUnaryB = Dict.fromList [(\"UnaryB1\", UnaryB1), (\"UnaryB2\", UnaryB2)]"
    ]

unaryASer :: String
unaryASer = unlines
    [ "jsonEncUnaryA  val ="
    , "    let keyval v = case v of"
    , "                    UnaryA1  -> (\"UnaryA1\", EValue (Json.Encode.list []))"
    , "                    UnaryA2  -> (\"UnaryA2\", EValue (Json.Encode.list []))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

unaryBSer :: String
unaryBSer = unlines
    [ "jsonEncUnaryB  val ="
    , "    case val of"
    , "        UnaryB1 -> Json.Encode.string \"UnaryB1\""
    , "        UnaryB2 -> Json.Encode.string \"UnaryB2\""
    ]

spec :: Spec
spec =
    describe "json serialisation" $
    do let rFoo = compileElmDef (Proxy :: Proxy Foo)
           rBar = compileElmDef (Proxy :: Proxy (Bar a))
           rBaz = compileElmDef (Proxy :: Proxy (Baz a))
           rQux = compileElmDef (Proxy :: Proxy (Qux a))
           rTest1 = compileElmDef (Proxy :: Proxy (TestComp a))
           rSomeOpts = compileElmDef (Proxy :: Proxy (SomeOpts a))
           rUnaryA = compileElmDef (Proxy :: Proxy UnaryA)
           rUnaryB = compileElmDef (Proxy :: Proxy UnaryB)
       it "should produce the correct ser code" $ do
             jsonSerForDef rFoo `shouldBe` fooSer
             jsonSerForDef rBar `shouldBe` barSer
             jsonSerForDef rSomeOpts `shouldBe` someOptsSer
             jsonSerForDef rBaz `shouldBe` bazSer
       it "should produce the correct ser code for unary unions" $ do
             jsonSerForDef rUnaryA `shouldBe` unaryASer
             jsonSerForDef rUnaryB `shouldBe` unaryBSer
       it "should produce the correct parse code for aliases" $ do
             jsonParserForDef rFoo `shouldBe` fooParse
             jsonParserForDef rBar `shouldBe` barParse
       it "should produce the correct parse code generic sum types" $ do
             jsonParserForDef rBaz `shouldBe` bazParse
             jsonParserForDef rSomeOpts `shouldBe` someOptsParse
             jsonParserForDef rTest1 `shouldBe` test1Parse
       it "should produce the correct parse code for unary unions" $ do
             jsonParserForDef rUnaryA `shouldBe` unaryAParse
             jsonParserForDef rUnaryB `shouldBe` unaryBParse
