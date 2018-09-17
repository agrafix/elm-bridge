{-# LANGUAGE TemplateHaskell #-}
module Elm.JsonSpec (spec) where

import Elm.Derive
import Elm.TyRender
import Elm.TyRep
import Elm.Json

import Data.Proxy
import Test.Hspec
import Data.Char (toLower)
import Data.Aeson.Types (defaultTaggedObject)
import qualified Data.Map.Strict as M
import qualified Data.Aeson.TH as TH

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
           | Testing (Baz a)

data TestComp a = TestComp { _t1 :: Change Int
                           , _t2 :: Change a
                           }

data DoneState   = Done | NotDone deriving (Eq, Show)

data Id = Id String deriving (Show, Eq)
data EditDone = EditDone Id DoneState DoneState deriving (Show, Eq)

newtype NTA = NTA Int
newtype NTB = NTB { getNtb :: Int }
newtype NTC = NTC Int
newtype NTD = NTD { getNtd :: Int }

newtype PhantomA a = PhantomA Int
newtype PhantomB a = PhantomB { getPhantomB :: Int }
newtype PhantomC a = PhantomC Int
newtype PhantomD a = PhantomD { getPhantomD :: Int }

$(deriveElmDef (defaultOptionsDropLower 2) ''Foo)
$(deriveElmDef (defaultOptionsDropLower 2) ''Bar)
$(deriveElmDef (defaultOptionsDropLower 1) ''TestComp)
$(deriveElmDef defaultOptions ''SomeOpts)
$(deriveElmDef defaultOptions{ allNullaryToStringTag = False } ''UnaryA)
$(deriveElmDef defaultOptions{ allNullaryToStringTag = True  } ''UnaryB)
$(deriveElmDef defaultOptions { fieldLabelModifier = drop 1 . map toLower } ''Baz)
$(deriveElmDef (defaultOptions { sumEncoding = defaultTaggedObject }) ''DoneState)
$(deriveElmDef (TH.defaultOptions { sumEncoding = TH.defaultTaggedObject }) ''Id)
$(deriveElmDef (TH.defaultOptions { sumEncoding = TH.defaultTaggedObject }) ''EditDone)
$(deriveElmDef defaultOptions ''NTA)
$(deriveElmDef defaultOptions ''NTB)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''NTC)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''NTD)
$(deriveElmDef defaultOptions ''PhantomA)
$(deriveElmDef defaultOptions ''PhantomB)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''PhantomC)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''PhantomD)

fooSer :: String
fooSer = "jsonEncFoo : Foo -> Value\njsonEncFoo  val =\n   Json.Encode.object\n   [ (\"name\", Json.Encode.string val.name)\n   , (\"blablub\", Json.Encode.int val.blablub)\n   ]\n"

fooParse :: String
fooParse = unlines
    [ "jsonDecFoo : Json.Decode.Decoder ( Foo )"
    , "jsonDecFoo ="
    , "   Json.Decode.succeed (\\pname pblablub -> {name = pname, blablub = pblablub})"
    , "   |> required \"name\" (Json.Decode.string)"
    , "   |> required \"blablub\" (Json.Decode.int)"
    ]

barSer :: String
barSer = unlines
    [ "jsonEncBar : (a -> Value) -> Bar a -> Value"
    , "jsonEncBar localEncoder_a val ="
    , "   Json.Encode.object"
    , "   [ (\"name\", localEncoder_a val.name)"
    , "   , (\"blablub\", Json.Encode.int val.blablub)"
    , "   , (\"tuple\", (\\(v1,v2) -> Json.Encode.list identity [(Json.Encode.int) v1,(Json.Encode.string) v2]) val.tuple)"
    , "   , (\"list\", (Json.Encode.list Json.Encode.bool) val.list)"
    , "   ]"
    ]

bazSer :: String
bazSer = unlines
    [ "jsonEncBaz : (a -> Value) -> Baz a -> Value"
    , "jsonEncBaz localEncoder_a val ="
    , "    let keyval v = case v of"
    , "                    Baz1 vs -> (\"Baz1\", encodeObject [(\"foo\", Json.Encode.int vs.foo), (\"qux\", (jsonEncMap (Json.Encode.int) (localEncoder_a)) vs.qux)])"
    , "                    Baz2 vs -> (\"Baz2\", encodeObject [(\"bar\", (maybeEncode (Json.Encode.int)) vs.bar), (\"str\", Json.Encode.string vs.str)])"
    , "                    Testing v1 -> (\"Testing\", encodeValue ((jsonEncBaz (localEncoder_a)) v1))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

barParse :: String
barParse = unlines
    [ "jsonDecBar : Json.Decode.Decoder a -> Json.Decode.Decoder ( Bar a )"
    , "jsonDecBar localDecoder_a ="
    , "   Json.Decode.succeed (\\pname pblablub ptuple plist -> {name = pname, blablub = pblablub, tuple = ptuple, list = plist})"
    , "   |> required \"name\" (localDecoder_a)"
    , "   |> required \"blablub\" (Json.Decode.int)"
    , "   |> required \"tuple\" (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.string)))"
    , "   |> required \"list\" (Json.Decode.list (Json.Decode.bool))"
    ]

bazParse :: String
bazParse = unlines
    [ "jsonDecBaz : Json.Decode.Decoder a -> Json.Decode.Decoder ( Baz a )"
    , "jsonDecBaz localDecoder_a ="
    , "    let jsonDecDictBaz = Dict.fromList"
    , "            [ (\"Baz1\", Json.Decode.lazy (\\_ -> Json.Decode.map Baz1 (   Json.Decode.succeed (\\pfoo pqux -> {foo = pfoo, qux = pqux})    |> required \"foo\" (Json.Decode.int)    |> required \"qux\" (jsonDecMap (Json.Decode.int) (localDecoder_a)))))"
    , "            , (\"Baz2\", Json.Decode.lazy (\\_ -> Json.Decode.map Baz2 (   Json.Decode.succeed (\\pbar pstr -> {bar = pbar, str = pstr})    |> fnullable \"bar\" (Json.Decode.int)    |> required \"str\" (Json.Decode.string))))"
    , "            , (\"Testing\", Json.Decode.lazy (\\_ -> Json.Decode.map Testing (jsonDecBaz (localDecoder_a))))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"Baz\" jsonDecDictBaz"
    ]

someOptsParse :: String
someOptsParse = unlines
    [ "jsonDecSomeOpts : Json.Decode.Decoder a -> Json.Decode.Decoder ( SomeOpts a )"
    , "jsonDecSomeOpts localDecoder_a ="
    , "    let jsonDecDictSomeOpts = Dict.fromList"
    , "            [ (\"Okay\", Json.Decode.lazy (\\_ -> Json.Decode.map Okay (Json.Decode.int)))"
    , "            , (\"NotOkay\", Json.Decode.lazy (\\_ -> Json.Decode.map NotOkay (localDecoder_a)))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"SomeOpts\" jsonDecDictSomeOpts"
    ]

someOptsSer :: String
someOptsSer = unlines
    [ "jsonEncSomeOpts : (a -> Value) -> SomeOpts a -> Value"
    , "jsonEncSomeOpts localEncoder_a val ="
    , "    let keyval v = case v of"
    , "                    Okay v1 -> (\"Okay\", encodeValue (Json.Encode.int v1))"
    , "                    NotOkay v1 -> (\"NotOkay\", encodeValue (localEncoder_a v1))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

test1Parse :: String
test1Parse = unlines
    [ "jsonDecTestComp : Json.Decode.Decoder a -> Json.Decode.Decoder ( TestComp a )"
    , "jsonDecTestComp localDecoder_a ="
    , "   Json.Decode.succeed (\\pt1 pt2 -> {t1 = pt1, t2 = pt2})"
    , "   |> required \"t1\" (jsonDecChange (Json.Decode.int))"
    , "   |> required \"t2\" (jsonDecChange (localDecoder_a))"
    ]

unaryAParse :: String
unaryAParse = unlines
    [ "jsonDecUnaryA : Json.Decode.Decoder ( UnaryA )"
    , "jsonDecUnaryA ="
    , "    let jsonDecDictUnaryA = Dict.fromList"
    , "            [ (\"UnaryA1\", Json.Decode.lazy (\\_ -> Json.Decode.succeed UnaryA1))"
    , "            , (\"UnaryA2\", Json.Decode.lazy (\\_ -> Json.Decode.succeed UnaryA2))"
    , "            ]"
    , "    in  decodeSumObjectWithSingleField  \"UnaryA\" jsonDecDictUnaryA"
    ]

unaryBParse :: String
unaryBParse = unlines
    [ "jsonDecUnaryB : Json.Decode.Decoder ( UnaryB )"
    , "jsonDecUnaryB = "
    , "    let jsonDecDictUnaryB = Dict.fromList [(\"UnaryB1\", UnaryB1), (\"UnaryB2\", UnaryB2)]"
    , "    in  decodeSumUnaries \"UnaryB\" jsonDecDictUnaryB"
    ]

unaryASer :: String
unaryASer = unlines
    [ "jsonEncUnaryA : UnaryA -> Value"
    , "jsonEncUnaryA  val ="
    , "    let keyval v = case v of"
    , "                    UnaryA1  -> (\"UnaryA1\", encodeValue (Json.Encode.list identity []))"
    , "                    UnaryA2  -> (\"UnaryA2\", encodeValue (Json.Encode.list identity []))"
    , "    in encodeSumObjectWithSingleField keyval val"
    ]

unaryBSer :: String
unaryBSer = unlines
    [ "jsonEncUnaryB : UnaryB -> Value"
    , "jsonEncUnaryB  val ="
    , "    case val of"
    , "        UnaryB1 -> Json.Encode.string \"UnaryB1\""
    , "        UnaryB2 -> Json.Encode.string \"UnaryB2\""
    ]

doneParse :: String
doneParse = unlines
  [ "jsonDecDoneState : Json.Decode.Decoder ( DoneState )"
  , "jsonDecDoneState = "
  , "    let jsonDecDictDoneState = Dict.fromList [(\"Done\", Done), (\"NotDone\", NotDone)]"
  , "    in  decodeSumUnaries \"DoneState\" jsonDecDictDoneState"
  ]

editDoneParse :: String
editDoneParse = unlines
  [ "jsonDecEditDone : Json.Decode.Decoder ( EditDone )"
  , "jsonDecEditDone ="
  , "    Json.Decode.lazy (\\_ -> Json.Decode.map3 EditDone (Json.Decode.index 0 (jsonDecId)) (Json.Decode.index 1 (jsonDecDoneState)) (Json.Decode.index 2 (jsonDecDoneState)))"
  , ""
  ]

idParse :: String
idParse = unlines
  [ "jsonDecId : Json.Decode.Decoder ( Id )"
  , "jsonDecId ="
  , "    Json.Decode.lazy (\\_ -> Json.Decode.map Id (Json.Decode.string))"
  , ""
  ]

ntaParse :: String
ntaParse = unlines
  [ "jsonDecNTA : Json.Decode.Decoder ( NTA )"
  , "jsonDecNTA ="
  , "    Json.Decode.int"
  ]

ntbParse :: String
ntbParse = unlines
  [ "jsonDecNTB : Json.Decode.Decoder ( NTB )"
  , "jsonDecNTB ="
  , "    Json.Decode.int"
  ]

ntcParse :: String
ntcParse = unlines
  [ "jsonDecNTC : Json.Decode.Decoder ( NTC )"
  , "jsonDecNTC ="
  , "    Json.Decode.int"
  ]

ntdParse :: String
ntdParse = unlines
  [ "jsonDecNTD : Json.Decode.Decoder ( NTD )"
  , "jsonDecNTD ="
  , "   Json.Decode.succeed (\\pgetNtd -> (NTD {getNtd = pgetNtd}))"
  , "   |> required \"getNtd\" (Json.Decode.int)"
  ]

phantomAParse :: String
phantomAParse = unlines
  [ "jsonDecPhantomA : Json.Decode.Decoder a -> Json.Decode.Decoder ( PhantomA a )"
  , "jsonDecPhantomA localDecoder_a ="
  , "    Json.Decode.int"
  ]
phantomBParse :: String
phantomBParse = unlines
  [ "jsonDecPhantomB : Json.Decode.Decoder a -> Json.Decode.Decoder ( PhantomB a )"
  , "jsonDecPhantomB localDecoder_a ="
  , "    Json.Decode.int"
  ]
phantomCParse :: String
phantomCParse = unlines
  [ "jsonDecPhantomC : Json.Decode.Decoder a -> Json.Decode.Decoder ( PhantomC a )"
  , "jsonDecPhantomC localDecoder_a ="
  , "    Json.Decode.int"
  ]
phantomDParse :: String
phantomDParse = unlines
  [ "jsonDecPhantomD : Json.Decode.Decoder a -> Json.Decode.Decoder ( PhantomD a )"
  , "jsonDecPhantomD localDecoder_a ="
  , "   Json.Decode.succeed (\\pgetPhantomD -> (PhantomD {getPhantomD = pgetPhantomD}))"
  , "   |> required \"getPhantomD\" (Json.Decode.int)"
  ]

spec :: Spec
spec =
    describe "json serialisation" $
    do let rFoo = compileElmDef (Proxy :: Proxy Foo)
           rBar = compileElmDef (Proxy :: Proxy (Bar a))
           rBaz = compileElmDef (Proxy :: Proxy (Baz a))
           rTest1 = compileElmDef (Proxy :: Proxy (TestComp a))
           rSomeOpts = compileElmDef (Proxy :: Proxy (SomeOpts a))
           rUnaryA = compileElmDef (Proxy :: Proxy UnaryA)
           rUnaryB = compileElmDef (Proxy :: Proxy UnaryB)
           rDoneState = compileElmDef (Proxy :: Proxy DoneState)
           rId = compileElmDef (Proxy :: Proxy Id)
           rEditDone = compileElmDef (Proxy :: Proxy EditDone)
           rNTA = compileElmDef (Proxy :: Proxy NTA)
           rNTB = compileElmDef (Proxy :: Proxy NTB)
           rNTC = compileElmDef (Proxy :: Proxy NTC)
           rNTD = compileElmDef (Proxy :: Proxy NTD)
           rPhantomA = compileElmDef (Proxy :: Proxy (PhantomA a))
           rPhantomB = compileElmDef (Proxy :: Proxy (PhantomB a))
           rPhantomC = compileElmDef (Proxy :: Proxy (PhantomC a))
           rPhantomD = compileElmDef (Proxy :: Proxy (PhantomD a))
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
       it "should produce the correct parse code for issue #18" $ do
             jsonParserForDef rDoneState `shouldBe` doneParse
             jsonParserForDef rId `shouldBe` idParse
             jsonParserForDef rEditDone `shouldBe` editDoneParse
       it "should produce the correct parse code for newtypes with unwrapUnaryRecords=True" $ do
            jsonParserForDef rNTA `shouldBe` ntaParse
            jsonParserForDef rNTB `shouldBe` ntbParse
       it "should produce the correct parse code for newtypes with unwrapUnaryRecords=False" $ do
            jsonParserForDef rNTC `shouldBe` ntcParse
            jsonParserForDef rNTD `shouldBe` ntdParse
       it "should produce the correct parse code for phantom newtypes" $ do
            jsonParserForDef rPhantomA `shouldBe` phantomAParse
            jsonParserForDef rPhantomB `shouldBe` phantomBParse
            jsonParserForDef rPhantomC `shouldBe` phantomCParse
            jsonParserForDef rPhantomD `shouldBe` phantomDParse
