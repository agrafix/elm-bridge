module MyTests exposing (..)
-- This module requires the following packages:
-- * bartavelle/json-helpers
-- * NoRedInk/elm-json-decode-pipeline
-- * elm/json
-- * elm-explorations/test

import Dict exposing (Dict, fromList)
import Expect exposing (Expectation, equal)
import Set exposing (Set)
import Json.Decode exposing (field, Value)
import Json.Encode
import Json.Helpers exposing (..)
import String
import Test exposing (Test, describe, test)

newtypeDecode : Test
newtypeDecode = describe "Newtype decoding checks"
              [ ntDecode1
              , ntDecode2
              , ntDecode3
              , ntDecode4
              ]

newtypeEncode : Test
newtypeEncode = describe "Newtype encoding checks"
              [ ntEncode1
              , ntEncode2
              , ntEncode3
              , ntEncode4
              ]

recordDecode : Test
recordDecode = describe "Record decoding checks"
              [ recordDecode1
              , recordDecode2
              ]

recordEncode : Test
recordEncode = describe "Record encoding checks"
              [ recordEncode1
              , recordEncode2
              ]

sumDecode : Test
sumDecode = describe "Sum decoding checks"
              [ sumDecode01
              , sumDecode02
              , sumDecode03
              , sumDecode04
              , sumDecode05
              , sumDecode06
              , sumDecode07
              , sumDecode08
              , sumDecode09
              , sumDecode10
              , sumDecode11
              , sumDecode12
              , sumDecodeUntagged
              , sumDecodeIncludeUnit
              ]

sumEncode : Test
sumEncode = describe "Sum encoding checks"
              [ sumEncode01
              , sumEncode02
              , sumEncode03
              , sumEncode04
              , sumEncode05
              , sumEncode06
              , sumEncode07
              , sumEncode08
              , sumEncode09
              , sumEncode10
              , sumEncode11
              , sumEncode12
              , sumEncodeUntagged
              , sumEncodeIncludeUnit
              ]

simpleDecode : Test
simpleDecode = describe "Simple records/types decode checks"
                [ simpleDecode01
                , simpleDecode02
                , simpleDecode03
                , simpleDecode04
                , simplerecordDecode01
                , simplerecordDecode02
                , simplerecordDecode03
                , simplerecordDecode04
                ]

simpleEncode : Test
simpleEncode = describe "Simple records/types encode checks"
                [ simpleEncode01
                , simpleEncode02
                , simpleEncode03
                , simpleEncode04
                , simplerecordEncode01
                , simplerecordEncode02
                , simplerecordEncode03
                , simplerecordEncode04
                ]

-- this is done to prevent artificial differences due to object ordering, this won't work with Maybe's though :(
equalHack : String -> String -> Expectation
equalHack a b =
    let remix = Json.Decode.decodeString Json.Decode.value
    in equal (remix a) (remix b)


type Record1 a = Record1
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   , jmap: (Dict String Int)
   }

jsonDecRecord1 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record1 a )
jsonDecRecord1 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux pjmap -> (Record1 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux, jmap = pjmap}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)
   |> required "jmap" (Json.Decode.dict (Json.Decode.int))

jsonEncRecord1 : (a -> Value) -> Record1 a -> Value
jsonEncRecord1 localEncoder_a (Record1 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   , ("jmap", (Json.Encode.dict identity (Json.Encode.int)) val.jmap)
   ]



type Record2 a = Record2
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   }

jsonDecRecord2 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record2 a )
jsonDecRecord2 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux -> (Record2 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)

jsonEncRecord2 : (a -> Value) -> Record2 a -> Value
jsonEncRecord2 localEncoder_a (Record2 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   ]



type Sum01 a =
    Sum01A a
    | Sum01B (Maybe a)
    | Sum01C a a
    | Sum01D {foo: a}
    | Sum01E {bar: Int, baz: Int}

jsonDecSum01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum01 a )
jsonDecSum01 localDecoder_a =
    let jsonDecDictSum01 = Dict.fromList
            [ ("Sum01A", Json.Decode.lazy (\_ -> Json.Decode.map Sum01A (localDecoder_a)))
            , ("Sum01B", Json.Decode.lazy (\_ -> Json.Decode.map Sum01B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum01C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum01C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum01D", Json.Decode.lazy (\_ -> Json.Decode.map Sum01D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum01E", Json.Decode.lazy (\_ -> Json.Decode.map Sum01E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum01 = Set.fromList ["Sum01D", "Sum01E"]
    in  decodeSumTaggedObject "Sum01" "tag" "content" jsonDecDictSum01 jsonDecObjectSetSum01

jsonEncSum01 : (a -> Value) -> Sum01 a -> Value
jsonEncSum01 localEncoder_a val =
    let keyval v = case v of
                    Sum01A v1 -> ("Sum01A", encodeValue (localEncoder_a v1))
                    Sum01B v1 -> ("Sum01B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum01C v1 v2 -> ("Sum01C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum01D vs -> ("Sum01D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum01E vs -> ("Sum01E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum02 a =
    Sum02A a
    | Sum02B (Maybe a)
    | Sum02C a a
    | Sum02D {foo: a}
    | Sum02E {bar: Int, baz: Int}

jsonDecSum02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum02 a )
jsonDecSum02 localDecoder_a =
    let jsonDecDictSum02 = Dict.fromList
            [ ("Sum02A", Json.Decode.lazy (\_ -> Json.Decode.map Sum02A (localDecoder_a)))
            , ("Sum02B", Json.Decode.lazy (\_ -> Json.Decode.map Sum02B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum02C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum02C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum02D", Json.Decode.lazy (\_ -> Json.Decode.map Sum02D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum02E", Json.Decode.lazy (\_ -> Json.Decode.map Sum02E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum02 = Set.fromList ["Sum02D", "Sum02E"]
    in  decodeSumTaggedObject "Sum02" "tag" "content" jsonDecDictSum02 jsonDecObjectSetSum02

jsonEncSum02 : (a -> Value) -> Sum02 a -> Value
jsonEncSum02 localEncoder_a val =
    let keyval v = case v of
                    Sum02A v1 -> ("Sum02A", encodeValue (localEncoder_a v1))
                    Sum02B v1 -> ("Sum02B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum02C v1 v2 -> ("Sum02C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum02D vs -> ("Sum02D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum02E vs -> ("Sum02E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum03 a =
    Sum03A a
    | Sum03B (Maybe a)
    | Sum03C a a
    | Sum03D {foo: a}
    | Sum03E {bar: Int, baz: Int}

jsonDecSum03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum03 a )
jsonDecSum03 localDecoder_a =
    let jsonDecDictSum03 = Dict.fromList
            [ ("Sum03A", Json.Decode.lazy (\_ -> Json.Decode.map Sum03A (localDecoder_a)))
            , ("Sum03B", Json.Decode.lazy (\_ -> Json.Decode.map Sum03B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum03C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum03C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum03D", Json.Decode.lazy (\_ -> Json.Decode.map Sum03D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum03E", Json.Decode.lazy (\_ -> Json.Decode.map Sum03E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum03 = Set.fromList ["Sum03D", "Sum03E"]
    in  decodeSumTaggedObject "Sum03" "tag" "content" jsonDecDictSum03 jsonDecObjectSetSum03

jsonEncSum03 : (a -> Value) -> Sum03 a -> Value
jsonEncSum03 localEncoder_a val =
    let keyval v = case v of
                    Sum03A v1 -> ("Sum03A", encodeValue (localEncoder_a v1))
                    Sum03B v1 -> ("Sum03B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum03C v1 v2 -> ("Sum03C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum03D vs -> ("Sum03D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum03E vs -> ("Sum03E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum04 a =
    Sum04A a
    | Sum04B (Maybe a)
    | Sum04C a a
    | Sum04D {foo: a}
    | Sum04E {bar: Int, baz: Int}

jsonDecSum04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum04 a )
jsonDecSum04 localDecoder_a =
    let jsonDecDictSum04 = Dict.fromList
            [ ("Sum04A", Json.Decode.lazy (\_ -> Json.Decode.map Sum04A (localDecoder_a)))
            , ("Sum04B", Json.Decode.lazy (\_ -> Json.Decode.map Sum04B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum04C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum04C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum04D", Json.Decode.lazy (\_ -> Json.Decode.map Sum04D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum04E", Json.Decode.lazy (\_ -> Json.Decode.map Sum04E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum04 = Set.fromList ["Sum04D", "Sum04E"]
    in  decodeSumTaggedObject "Sum04" "tag" "content" jsonDecDictSum04 jsonDecObjectSetSum04

jsonEncSum04 : (a -> Value) -> Sum04 a -> Value
jsonEncSum04 localEncoder_a val =
    let keyval v = case v of
                    Sum04A v1 -> ("Sum04A", encodeValue (localEncoder_a v1))
                    Sum04B v1 -> ("Sum04B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum04C v1 v2 -> ("Sum04C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum04D vs -> ("Sum04D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum04E vs -> ("Sum04E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum05 a =
    Sum05A a
    | Sum05B (Maybe a)
    | Sum05C a a
    | Sum05D {foo: a}
    | Sum05E {bar: Int, baz: Int}

jsonDecSum05 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum05 a )
jsonDecSum05 localDecoder_a =
    let jsonDecDictSum05 = Dict.fromList
            [ ("Sum05A", Json.Decode.lazy (\_ -> Json.Decode.map Sum05A (localDecoder_a)))
            , ("Sum05B", Json.Decode.lazy (\_ -> Json.Decode.map Sum05B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum05C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum05C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum05D", Json.Decode.lazy (\_ -> Json.Decode.map Sum05D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum05E", Json.Decode.lazy (\_ -> Json.Decode.map Sum05E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum05" jsonDecDictSum05

jsonEncSum05 : (a -> Value) -> Sum05 a -> Value
jsonEncSum05 localEncoder_a val =
    let keyval v = case v of
                    Sum05A v1 -> ("Sum05A", encodeValue (localEncoder_a v1))
                    Sum05B v1 -> ("Sum05B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum05C v1 v2 -> ("Sum05C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum05D vs -> ("Sum05D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum05E vs -> ("Sum05E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum06 a =
    Sum06A a
    | Sum06B (Maybe a)
    | Sum06C a a
    | Sum06D {foo: a}
    | Sum06E {bar: Int, baz: Int}

jsonDecSum06 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum06 a )
jsonDecSum06 localDecoder_a =
    let jsonDecDictSum06 = Dict.fromList
            [ ("Sum06A", Json.Decode.lazy (\_ -> Json.Decode.map Sum06A (localDecoder_a)))
            , ("Sum06B", Json.Decode.lazy (\_ -> Json.Decode.map Sum06B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum06C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum06C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum06D", Json.Decode.lazy (\_ -> Json.Decode.map Sum06D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum06E", Json.Decode.lazy (\_ -> Json.Decode.map Sum06E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum06" jsonDecDictSum06

jsonEncSum06 : (a -> Value) -> Sum06 a -> Value
jsonEncSum06 localEncoder_a val =
    let keyval v = case v of
                    Sum06A v1 -> ("Sum06A", encodeValue (localEncoder_a v1))
                    Sum06B v1 -> ("Sum06B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum06C v1 v2 -> ("Sum06C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum06D vs -> ("Sum06D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum06E vs -> ("Sum06E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum07 a =
    Sum07A a
    | Sum07B (Maybe a)
    | Sum07C a a
    | Sum07D {foo: a}
    | Sum07E {bar: Int, baz: Int}

jsonDecSum07 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum07 a )
jsonDecSum07 localDecoder_a =
    let jsonDecDictSum07 = Dict.fromList
            [ ("Sum07A", Json.Decode.lazy (\_ -> Json.Decode.map Sum07A (localDecoder_a)))
            , ("Sum07B", Json.Decode.lazy (\_ -> Json.Decode.map Sum07B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum07C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum07C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum07D", Json.Decode.lazy (\_ -> Json.Decode.map Sum07D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum07E", Json.Decode.lazy (\_ -> Json.Decode.map Sum07E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum07" jsonDecDictSum07

jsonEncSum07 : (a -> Value) -> Sum07 a -> Value
jsonEncSum07 localEncoder_a val =
    let keyval v = case v of
                    Sum07A v1 -> ("Sum07A", encodeValue (localEncoder_a v1))
                    Sum07B v1 -> ("Sum07B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum07C v1 v2 -> ("Sum07C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum07D vs -> ("Sum07D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum07E vs -> ("Sum07E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum08 a =
    Sum08A a
    | Sum08B (Maybe a)
    | Sum08C a a
    | Sum08D {foo: a}
    | Sum08E {bar: Int, baz: Int}

jsonDecSum08 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum08 a )
jsonDecSum08 localDecoder_a =
    let jsonDecDictSum08 = Dict.fromList
            [ ("Sum08A", Json.Decode.lazy (\_ -> Json.Decode.map Sum08A (localDecoder_a)))
            , ("Sum08B", Json.Decode.lazy (\_ -> Json.Decode.map Sum08B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum08C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum08C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum08D", Json.Decode.lazy (\_ -> Json.Decode.map Sum08D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum08E", Json.Decode.lazy (\_ -> Json.Decode.map Sum08E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum08" jsonDecDictSum08

jsonEncSum08 : (a -> Value) -> Sum08 a -> Value
jsonEncSum08 localEncoder_a val =
    let keyval v = case v of
                    Sum08A v1 -> ("Sum08A", encodeValue (localEncoder_a v1))
                    Sum08B v1 -> ("Sum08B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum08C v1 v2 -> ("Sum08C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum08D vs -> ("Sum08D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum08E vs -> ("Sum08E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum09 a =
    Sum09A a
    | Sum09B (Maybe a)
    | Sum09C a a
    | Sum09D {foo: a}
    | Sum09E {bar: Int, baz: Int}

jsonDecSum09 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum09 a )
jsonDecSum09 localDecoder_a =
    let jsonDecDictSum09 = Dict.fromList
            [ ("Sum09A", Json.Decode.lazy (\_ -> Json.Decode.map Sum09A (localDecoder_a)))
            , ("Sum09B", Json.Decode.lazy (\_ -> Json.Decode.map Sum09B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum09C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum09C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum09D", Json.Decode.lazy (\_ -> Json.Decode.map Sum09D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum09E", Json.Decode.lazy (\_ -> Json.Decode.map Sum09E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum09" jsonDecDictSum09

jsonEncSum09 : (a -> Value) -> Sum09 a -> Value
jsonEncSum09 localEncoder_a val =
    let keyval v = case v of
                    Sum09A v1 -> ("Sum09A", encodeValue (localEncoder_a v1))
                    Sum09B v1 -> ("Sum09B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum09C v1 v2 -> ("Sum09C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum09D vs -> ("Sum09D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum09E vs -> ("Sum09E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum10 a =
    Sum10A a
    | Sum10B (Maybe a)
    | Sum10C a a
    | Sum10D {foo: a}
    | Sum10E {bar: Int, baz: Int}

jsonDecSum10 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum10 a )
jsonDecSum10 localDecoder_a =
    let jsonDecDictSum10 = Dict.fromList
            [ ("Sum10A", Json.Decode.lazy (\_ -> Json.Decode.map Sum10A (localDecoder_a)))
            , ("Sum10B", Json.Decode.lazy (\_ -> Json.Decode.map Sum10B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum10C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum10C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum10D", Json.Decode.lazy (\_ -> Json.Decode.map Sum10D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum10E", Json.Decode.lazy (\_ -> Json.Decode.map Sum10E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum10" jsonDecDictSum10

jsonEncSum10 : (a -> Value) -> Sum10 a -> Value
jsonEncSum10 localEncoder_a val =
    let keyval v = case v of
                    Sum10A v1 -> ("Sum10A", encodeValue (localEncoder_a v1))
                    Sum10B v1 -> ("Sum10B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum10C v1 v2 -> ("Sum10C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum10D vs -> ("Sum10D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum10E vs -> ("Sum10E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum11 a =
    Sum11A a
    | Sum11B (Maybe a)
    | Sum11C a a
    | Sum11D {foo: a}
    | Sum11E {bar: Int, baz: Int}

jsonDecSum11 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum11 a )
jsonDecSum11 localDecoder_a =
    let jsonDecDictSum11 = Dict.fromList
            [ ("Sum11A", Json.Decode.lazy (\_ -> Json.Decode.map Sum11A (localDecoder_a)))
            , ("Sum11B", Json.Decode.lazy (\_ -> Json.Decode.map Sum11B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum11C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum11C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum11D", Json.Decode.lazy (\_ -> Json.Decode.map Sum11D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum11E", Json.Decode.lazy (\_ -> Json.Decode.map Sum11E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum11" jsonDecDictSum11

jsonEncSum11 : (a -> Value) -> Sum11 a -> Value
jsonEncSum11 localEncoder_a val =
    let keyval v = case v of
                    Sum11A v1 -> ("Sum11A", encodeValue (localEncoder_a v1))
                    Sum11B v1 -> ("Sum11B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum11C v1 v2 -> ("Sum11C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum11D vs -> ("Sum11D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum11E vs -> ("Sum11E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum12 a =
    Sum12A a
    | Sum12B (Maybe a)
    | Sum12C a a
    | Sum12D {foo: a}
    | Sum12E {bar: Int, baz: Int}

jsonDecSum12 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum12 a )
jsonDecSum12 localDecoder_a =
    let jsonDecDictSum12 = Dict.fromList
            [ ("Sum12A", Json.Decode.lazy (\_ -> Json.Decode.map Sum12A (localDecoder_a)))
            , ("Sum12B", Json.Decode.lazy (\_ -> Json.Decode.map Sum12B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum12C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum12C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum12D", Json.Decode.lazy (\_ -> Json.Decode.map Sum12D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum12E", Json.Decode.lazy (\_ -> Json.Decode.map Sum12E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum12" jsonDecDictSum12

jsonEncSum12 : (a -> Value) -> Sum12 a -> Value
jsonEncSum12 localEncoder_a val =
    let keyval v = case v of
                    Sum12A v1 -> ("Sum12A", encodeValue (localEncoder_a v1))
                    Sum12B v1 -> ("Sum12B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum12C v1 v2 -> ("Sum12C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum12D vs -> ("Sum12D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum12E vs -> ("Sum12E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Simple01 a =
    Simple01 a

jsonDecSimple01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple01 a )
jsonDecSimple01 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple01 (localDecoder_a))


jsonEncSimple01 : (a -> Value) -> Simple01 a -> Value
jsonEncSimple01 localEncoder_a(Simple01 v1) =
    localEncoder_a v1



type Simple02 a =
    Simple02 a

jsonDecSimple02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple02 a )
jsonDecSimple02 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple02 (localDecoder_a))


jsonEncSimple02 : (a -> Value) -> Simple02 a -> Value
jsonEncSimple02 localEncoder_a(Simple02 v1) =
    localEncoder_a v1



type Simple03 a =
    Simple03 a

jsonDecSimple03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple03 a )
jsonDecSimple03 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple03 (localDecoder_a))


jsonEncSimple03 : (a -> Value) -> Simple03 a -> Value
jsonEncSimple03 localEncoder_a(Simple03 v1) =
    localEncoder_a v1



type Simple04 a =
    Simple04 a

jsonDecSimple04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple04 a )
jsonDecSimple04 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple04 (localDecoder_a))


jsonEncSimple04 : (a -> Value) -> Simple04 a -> Value
jsonEncSimple04 localEncoder_a(Simple04 v1) =
    localEncoder_a v1



type SimpleRecord01 a = SimpleRecord01
   { qux: a
   }

jsonDecSimpleRecord01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord01 a )
jsonDecSimpleRecord01 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord01 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord01 : (a -> Value) -> SimpleRecord01 a -> Value
jsonEncSimpleRecord01 localEncoder_a (SimpleRecord01 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord02 a = SimpleRecord02
   { qux: a
   }

jsonDecSimpleRecord02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord02 a )
jsonDecSimpleRecord02 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord02 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord02 : (a -> Value) -> SimpleRecord02 a -> Value
jsonEncSimpleRecord02 localEncoder_a (SimpleRecord02 val) =
   localEncoder_a val.qux


type SimpleRecord03 a = SimpleRecord03
   { qux: a
   }

jsonDecSimpleRecord03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord03 a )
jsonDecSimpleRecord03 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord03 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord03 : (a -> Value) -> SimpleRecord03 a -> Value
jsonEncSimpleRecord03 localEncoder_a (SimpleRecord03 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord04 a = SimpleRecord04
   { qux: a
   }

jsonDecSimpleRecord04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord04 a )
jsonDecSimpleRecord04 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord04 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord04 : (a -> Value) -> SimpleRecord04 a -> Value
jsonEncSimpleRecord04 localEncoder_a (SimpleRecord04 val) =
   localEncoder_a val.qux


type SumUntagged a =
    SMInt Int
    | SMList a

jsonDecSumUntagged : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumUntagged a )
jsonDecSumUntagged localDecoder_a =
    let jsonDecDictSumUntagged = Dict.fromList
            [ ("SMInt", Json.Decode.lazy (\_ -> Json.Decode.map SMInt (Json.Decode.int)))
            , ("SMList", Json.Decode.lazy (\_ -> Json.Decode.map SMList (localDecoder_a)))
            ]
    in  Json.Decode.oneOf (Dict.values jsonDecDictSumUntagged)

jsonEncSumUntagged : (a -> Value) -> SumUntagged a -> Value
jsonEncSumUntagged localEncoder_a val =
    let keyval v = case v of
                    SMInt v1 -> ("SMInt", encodeValue (Json.Encode.int v1))
                    SMList v1 -> ("SMList", encodeValue (localEncoder_a v1))
    in encodeSumUntagged keyval val



type SumIncludeUnit a =
    SumIncludeUnitZero 
    | SumIncludeUnitOne a
    | SumIncludeUnitTwo a a

jsonDecSumIncludeUnit : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumIncludeUnit a )
jsonDecSumIncludeUnit localDecoder_a =
    let jsonDecDictSumIncludeUnit = Dict.fromList
            [ ("SumIncludeUnitZero", Json.Decode.lazy (\_ -> Json.Decode.succeed SumIncludeUnitZero))
            , ("SumIncludeUnitOne", Json.Decode.lazy (\_ -> Json.Decode.map SumIncludeUnitOne (localDecoder_a)))
            , ("SumIncludeUnitTwo", Json.Decode.lazy (\_ -> Json.Decode.map2 SumIncludeUnitTwo (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            ]
        jsonDecObjectSetSumIncludeUnit = Set.fromList ["SumIncludeUnitZero"]
    in  decodeSumTaggedObject "SumIncludeUnit" "tag" "content" jsonDecDictSumIncludeUnit jsonDecObjectSetSumIncludeUnit

jsonEncSumIncludeUnit : (a -> Value) -> SumIncludeUnit a -> Value
jsonEncSumIncludeUnit localEncoder_a val =
    let keyval v = case v of
                    SumIncludeUnitZero  -> ("SumIncludeUnitZero", encodeValue (Json.Encode.list identity []))
                    SumIncludeUnitOne v1 -> ("SumIncludeUnitOne", encodeValue (localEncoder_a v1))
                    SumIncludeUnitTwo v1 v2 -> ("SumIncludeUnitTwo", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
    in encodeSumTaggedObject "tag" "content" keyval val



type alias NT1  = (List Int)

jsonDecNT1 : Json.Decode.Decoder ( NT1 )
jsonDecNT1 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT1 : NT1 -> Value
jsonEncNT1  val = (Json.Encode.list Json.Encode.int) val



type alias NT2  = (List Int)

jsonDecNT2 : Json.Decode.Decoder ( NT2 )
jsonDecNT2 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT2 : NT2 -> Value
jsonEncNT2  val = (Json.Encode.list Json.Encode.int) val



type alias NT3  = (List Int)

jsonDecNT3 : Json.Decode.Decoder ( NT3 )
jsonDecNT3 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT3 : NT3 -> Value
jsonEncNT3  val = (Json.Encode.list Json.Encode.int) val



type NT4  = NT4
   { foo: (List Int)
   }

jsonDecNT4 : Json.Decode.Decoder ( NT4 )
jsonDecNT4 =
   Json.Decode.succeed (\pfoo -> (NT4 {foo = pfoo}))
   |> required "foo" (Json.Decode.list (Json.Decode.int))

jsonEncNT4 : NT4 -> Value
jsonEncNT4  (NT4 val) =
   Json.Encode.object
   [ ("foo", (Json.Encode.list Json.Encode.int) val.foo)
   ]



sumEncode01 : Test
sumEncode01 = describe "Sum encode 01"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-1,1]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-1,1]))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[-1,2,4,-3],[]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [-1,2,4,-3] []))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[6,5,3]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = [6,5,3]}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[2,-8,4]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = [2,-8,4]}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[-2,-8,-6]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [-2,-8,-6])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[-8,1,-10,-1,2],[9,4,5,-12,2,-5,8]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [-8,1,-10,-1,2] [9,4,5,-12,2,-5,8]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-14,1,-9,-2,0,-1,7,-14,-14,5,-10]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-14,1,-9,-2,0,-1,7,-14,-14,5,-10]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[13,15,-14,3,0,2,-9,4,-1,-2,-5,-2,3,-15,5]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [13,15,-14,3,0,2,-9,4,-1,-2,-5,-2,3,-15,5]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":-17,\"baz\":13}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = -17, baz = 13}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[19,5,5,3,-12,-9,14,18,-18,-2,-15,-11,-16,13,-20],[-13,-3,-13,-15,-2]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [19,5,5,3,-12,-9,14,18,-18,-2,-15,-11,-16,13,-20] [-13,-3,-13,-15,-2]))))
  ]

sumEncode02 : Test
sumEncode02 = describe "Sum encode 02"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[-2,2]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [-2,2]}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[3,-2]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [3,-2]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[-3,3,4]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [-3,3,4]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[-2,-4,-1,-1,5,-7]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [-2,-4,-1,-1,5,-7]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-1,\"baz\":10}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = -1, baz = 10}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":8,\"baz\":12}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 8, baz = 12}))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-9,\"baz\":-13}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = -9, baz = -13}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[-9],[]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [-9] []))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[14,4,5,-2,-2]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [14,4,5,-2,-2]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[19,17,14,14,1,-20,-4,9,2,3,2,4,16,-19,-15]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [19,17,14,14,1,-20,-4,9,2,3,2,4,16,-19,-15]}))))
  ]

sumEncode03 : Test
sumEncode03 = describe "Sum encode 03"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-1],[]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [-1] []))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-2,3,1,-4],[1,-1,1]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [-2,3,1,-4] [1,-1,1]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":3,\"baz\":-4}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 3, baz = -4}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-3,-1,5,-4]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = [-3,-1,5,-4]}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[10,-3]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [10,-3])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-4,-5,-8],[-5]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [-4,-5,-8] [-5]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[5,5,1,2,0,5,-12,13,-2,-3,-4,11,4,12]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [5,5,1,2,0,5,-12,13,-2,-3,-4,11,4,12]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A []))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-16,-6,10,16,-9,-15,6,14,-12,9,1,-6,17,12,-12]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = [-16,-6,10,16,-9,-15,6,14,-12,9,1,-6,17,12,-12]}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[11,19,20,-8,12,19,-14,10,2,-14,1,4,-9],[14,-12,-12,2,-11,7,14,17,-1,8,7,2,-18,6,4,13,13,5]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [11,19,20,-8,12,19,-14,10,2,-14,1,4,-9] [14,-12,-12,2,-11,7,14,17,-1,8,7,2,-18,6,4,13,13,5]))))
  ]

sumEncode04 : Test
sumEncode04 = describe "Sum encode 04"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[-2,1],[]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [-2,1] []))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-3,-1,1]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [-3,-1,1])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":-5,\"baz\":4}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = -5, baz = 4}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[0,-8,-5,-5,6],[-2,8,-3]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [0,-8,-5,-5,6] [-2,8,-3]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[8,-4,2,8,-2,9,4,6,4,1]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04A [8,-4,2,8,-2,9,4,6,4,1]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[0,4,11,-9,-4,9,2,9,7,7,8],[-4,-2,12,9,-3,7,-6,7,-12,8]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [0,4,11,-9,-4,9,2,9,7,7,8] [-4,-2,12,9,-3,7,-6,7,-12,8]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[6,-1,-9,-4,14,6,-6,9,8,1,7,13]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [] [6,-1,-9,-4,14,6,-6,9,8,1,7,13]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[2,6,2,6,13,-4,11]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [2,6,2,6,13,-4,11]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":4,\"baz\":-10}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = 4, baz = -10}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[-18,15,-10,-20,-18,7,-5,12,-11,19,17,3,14,8,-6,1,-5,-15]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [-18,15,-10,-20,-18,7,-5,12,-11,19,17,3,14,8,-6,1,-5,-15]}))))
  ]

sumEncode05 : Test
sumEncode05 = describe "Sum encode 05"
  [ test "1" (\_ -> equalHack "{\"Sum05B\":[]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [])))))
  , test "2" (\_ -> equalHack "{\"Sum05C\":[[0],[-2,0]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [0] [-2,0]))))
  , test "3" (\_ -> equalHack "{\"Sum05A\":[3,-2,4,0]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [3,-2,4,0]))))
  , test "4" (\_ -> equalHack "{\"Sum05B\":[4,-5,3,6,2]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [4,-5,3,6,2])))))
  , test "5" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[5,8,-5,-8,8,3,4]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [5,8,-5,-8,8,3,4]}))))
  , test "6" (\_ -> equalHack "{\"Sum05C\":[[-2,3],[-8,8]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [-2,3] [-8,8]))))
  , test "7" (\_ -> equalHack "{\"Sum05A\":[-10,-7]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [-10,-7]))))
  , test "8" (\_ -> equalHack "{\"Sum05A\":[-8,8,7,-10]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [-8,8,7,-10]))))
  , test "9" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[2,-1,1,-6,-8,-1,1,-2,16,4,7,-1,-13]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [2,-1,1,-6,-8,-1,1,-2,16,4,7,-1,-13]}))))
  , test "10" (\_ -> equalHack "{\"Sum05C\":[[-4,3,-2,3,1,17,16,4,-2,-5,-10],[11,18,-8,7,3]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [-4,3,-2,3,1,17,16,4,-2,-5,-10] [11,18,-8,7,3]))))
  , test "11" (\_ -> equalHack "{\"Sum05E\":{\"bar\":18,\"baz\":8}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = 18, baz = 8}))))
  ]

sumEncode06 : Test
sumEncode06 = describe "Sum encode 06"
  [ test "1" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum06E\":{\"bar\":1,\"baz\":2}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 1, baz = 2}))))
  , test "3" (\_ -> equalHack "{\"Sum06B\":[-1]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [-1])))))
  , test "4" (\_ -> equalHack "{\"Sum06B\":[5,5,3]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [5,5,3])))))
  , test "5" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[5,-4]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [5,-4]}))))
  , test "6" (\_ -> equalHack "{\"Sum06B\":[3,3]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [3,3])))))
  , test "7" (\_ -> equalHack "{\"Sum06A\":[-12,-3,0,-7,-4,0,-9,0]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A [-12,-3,0,-7,-4,0,-9,0]))))
  , test "8" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[0,0,-6,7,-14,-12,-10,1,3,-14,-6,-10]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [0,0,-6,7,-14,-12,-10,1,3,-14,-6,-10]}))))
  , test "9" (\_ -> equalHack "{\"Sum06B\":[14,6,5,-14,5,-4,16,3,-11,1,-5,-7,12,4,-1,-6]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [14,6,5,-14,5,-4,16,3,-11,1,-5,-7,12,4,-1,-6])))))
  , test "10" (\_ -> equalHack "{\"Sum06C\":[[16,7,17,18,-15,15,-15,-4,1,-10,-8,11,-7,-8],[11,9,-13,-3,-17,12]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [16,7,17,18,-15,15,-15,-4,1,-10,-8,11,-7,-8] [11,9,-13,-3,-17,12]))))
  , test "11" (\_ -> equalHack "{\"Sum06C\":[[-14,20,-11,18,5,1,-12,-8,-15,-9,-5],[0,7]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [-14,20,-11,18,5,1,-12,-8,-15,-9,-5] [0,7]))))
  ]

sumEncode07 : Test
sumEncode07 = describe "Sum encode 07"
  [ test "1" (\_ -> equalHack "{\"Sum07A\":[]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A []))))
  , test "2" (\_ -> equalHack "{\"Sum07E\":{\"bar\":-2,\"baz\":2}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = -2, baz = 2}))))
  , test "3" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[-4,-2]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [-4,-2]}))))
  , test "4" (\_ -> equalHack "{\"Sum07C\":[[],[1]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [] [1]))))
  , test "5" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[-6,7]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [-6,7]}))))
  , test "6" (\_ -> equalHack "{\"Sum07A\":[9,8,-6,-3,-8]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [9,8,-6,-3,-8]))))
  , test "7" (\_ -> equalHack "{\"Sum07C\":[[-12,-4,-12,0,10,0,7],[-7,-3,7,-8,6,-9,1,4]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [-12,-4,-12,0,10,0,7] [-7,-3,7,-8,6,-9,1,4]))))
  , test "8" (\_ -> equalHack "{\"Sum07C\":[[8,9,-7,2,12,12,-9,0,7,-6,4],[-11,11,-9,-11,-2,-10,-9,-14,-3]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [8,9,-7,2,12,12,-9,0,7,-6,4] [-11,11,-9,-11,-2,-10,-9,-14,-3]))))
  , test "9" (\_ -> equalHack "{\"Sum07A\":[-12,12,-1,-16]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [-12,12,-1,-16]))))
  , test "10" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[15,2,6,14,1,-17,-7,6,8]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [15,2,6,14,1,-17,-7,6,8]}))))
  , test "11" (\_ -> equalHack "{\"Sum07B\":[-10,14,-19,-8,-2,17]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-10,14,-19,-8,-2,17])))))
  ]

sumEncode08 : Test
sumEncode08 = describe "Sum encode 08"
  [ test "1" (\_ -> equalHack "{\"Sum08E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"Sum08E\":{\"bar\":-2,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = -2, baz = 0}))))
  , test "3" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[-3,-4]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [-3,-4]}))))
  , test "4" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[5,-3]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [5,-3]}))))
  , test "5" (\_ -> equalHack "{\"Sum08E\":{\"bar\":3,\"baz\":-1}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 3, baz = -1}))))
  , test "6" (\_ -> equalHack "{\"Sum08A\":[6,-9,5,6,3,-2]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [6,-9,5,6,3,-2]))))
  , test "7" (\_ -> equalHack "{\"Sum08C\":[[-5,9,12],[8,6,-3]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [-5,9,12] [8,6,-3]))))
  , test "8" (\_ -> equalHack "{\"Sum08C\":[[],[2,1,-12,3,7,2]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [] [2,1,-12,3,7,2]))))
  , test "9" (\_ -> equalHack "{\"Sum08A\":[-5,-2,-14,14,2,-9,-15,-10,5]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [-5,-2,-14,14,2,-9,-15,-10,5]))))
  , test "10" (\_ -> equalHack "{\"Sum08C\":[[14,-5,14,18,4,14,-6,-1,-11,-10],[-17,0,12,10,-5,-18,17]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [14,-5,14,18,4,14,-6,-1,-11,-10] [-17,0,12,10,-5,-18,17]))))
  , test "11" (\_ -> equalHack "{\"Sum08C\":[[-14,-9,-18,2],[18,-12,0]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [-14,-9,-18,2] [18,-12,0]))))
  ]

sumEncode09 : Test
sumEncode09 = describe "Sum encode 09"
  [ test "1" (\_ -> equalHack "[\"Sum09E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "[\"Sum09C\",[[2],[]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [2] []))))
  , test "3" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-3,\"baz\":-2}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = -3, baz = -2}))))
  , test "4" (\_ -> equalHack "[\"Sum09C\",[[-2,1,-2,-3,2],[1,-2,0,-2]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [-2,1,-2,-3,2] [1,-2,0,-2]))))
  , test "5" (\_ -> equalHack "[\"Sum09A\",[3,-7,-2,0,3]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [3,-7,-2,0,3]))))
  , test "6" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[-2,5,-9,7,0,10,7,10,-1]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [-2,5,-9,7,0,10,7,10,-1]}))))
  , test "7" (\_ -> equalHack "[\"Sum09C\",[[-10,-7,-7,-11,-9,8,-6],[12]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [-10,-7,-7,-11,-9,8,-6] [12]))))
  , test "8" (\_ -> equalHack "[\"Sum09B\",[8,10,-5,7,-3,-2,-8,-1,-11,14]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [8,10,-5,7,-3,-2,-8,-1,-11,14])))))
  , test "9" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-12,\"baz\":3}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = -12, baz = 3}))))
  , test "10" (\_ -> equalHack "[\"Sum09B\",[14,5,-18,-7,-18,-16,2,8,4,7,-8,5,10,10]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [14,5,-18,-7,-18,-16,2,8,4,7,-8,5,10,10])))))
  , test "11" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-15,\"baz\":18}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = -15, baz = 18}))))
  ]

sumEncode10 : Test
sumEncode10 = describe "Sum encode 10"
  [ test "1" (\_ -> equalHack "[\"Sum10C\",[[],[]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [] []))))
  , test "2" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[1]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10D {foo = [1]}))))
  , test "3" (\_ -> equalHack "[\"Sum10E\",{\"bar\":0,\"baz\":-2}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 0, baz = -2}))))
  , test "4" (\_ -> equalHack "[\"Sum10E\",{\"bar\":-1,\"baz\":2}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = -1, baz = 2}))))
  , test "5" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[5,-3,7,0,-2,-4]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10D {foo = [5,-3,7,0,-2,-4]}))))
  , test "6" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[-8,-6,7,0,5]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10D {foo = [-8,-6,7,0,5]}))))
  , test "7" (\_ -> equalHack "[\"Sum10B\",[-1,-7,0,6,7,2,-12,12,-8,-12]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [-1,-7,0,6,7,2,-12,12,-8,-12])))))
  , test "8" (\_ -> equalHack "[\"Sum10A\",[-12,0,6,12,-14,14,-6,8,10,14,7]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [-12,0,6,12,-14,14,-6,8,10,14,7]))))
  , test "9" (\_ -> equalHack "[\"Sum10A\",[]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A []))))
  , test "10" (\_ -> equalHack "[\"Sum10E\",{\"bar\":16,\"baz\":14}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 16, baz = 14}))))
  , test "11" (\_ -> equalHack "[\"Sum10A\",[10,-14,6,9,-15,7,5,13,-1,10,-10,-6,-6,5]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [10,-14,6,9,-15,7,5,13,-1,10,-10,-6,-6,5]))))
  ]

sumEncode11 : Test
sumEncode11 = describe "Sum encode 11"
  [ test "1" (\_ -> equalHack "[\"Sum11C\",[[],[]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [] []))))
  , test "2" (\_ -> equalHack "[\"Sum11A\",[1,0]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [1,0]))))
  , test "3" (\_ -> equalHack "[\"Sum11B\",[3,3,2]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [3,3,2])))))
  , test "4" (\_ -> equalHack "[\"Sum11E\",{\"bar\":4,\"baz\":-1}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 4, baz = -1}))))
  , test "5" (\_ -> equalHack "[\"Sum11A\",[3,1,1,3,-5,4,5,-7]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [3,1,1,3,-5,4,5,-7]))))
  , test "6" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[2,-8,1,-5,-1,-10]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [2,-8,1,-5,-1,-10]}))))
  , test "7" (\_ -> equalHack "[\"Sum11E\",{\"bar\":0,\"baz\":12}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 0, baz = 12}))))
  , test "8" (\_ -> equalHack "[\"Sum11E\",{\"bar\":-10,\"baz\":-6}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = -10, baz = -6}))))
  , test "9" (\_ -> equalHack "[\"Sum11A\",[4]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [4]))))
  , test "10" (\_ -> equalHack "[\"Sum11C\",[[],[16,8,-7,16,18,-6,7,1,-8,15,-6,1,-1]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [] [16,8,-7,16,18,-6,7,1,-8,15,-6,1,-1]))))
  , test "11" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[-16,6,14,2,-11,8,13,13,-13,-15]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [-16,6,14,2,-11,8,13,13,-13,-15]}))))
  ]

sumEncode12 : Test
sumEncode12 = describe "Sum encode 12"
  [ test "1" (\_ -> equalHack "[\"Sum12B\",[]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [])))))
  , test "2" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = []}))))
  , test "3" (\_ -> equalHack "[\"Sum12B\",[-1]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [-1])))))
  , test "4" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[6,5,0]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [6,5,0]}))))
  , test "5" (\_ -> equalHack "[\"Sum12E\",{\"bar\":4,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = 4, baz = 0}))))
  , test "6" (\_ -> equalHack "[\"Sum12A\",[-5,4,-7,-9,-4,-8,-9,8,6,-5]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [-5,4,-7,-9,-4,-8,-9,8,6,-5]))))
  , test "7" (\_ -> equalHack "[\"Sum12E\",{\"bar\":-7,\"baz\":-7}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = -7, baz = -7}))))
  , test "8" (\_ -> equalHack "[\"Sum12C\",[[4,-2,1,-6,14,3,-2,-14,9],[-1,-6,-13,-7,2,-4,-6,-13,-10,-5,14,3,6]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [4,-2,1,-6,14,3,-2,-14,9] [-1,-6,-13,-7,2,-4,-6,-13,-10,-5,14,3,6]))))
  , test "9" (\_ -> equalHack "[\"Sum12B\",[-11,-12,15,7,-12,9,-6,9]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [-11,-12,15,7,-12,9,-6,9])))))
  , test "10" (\_ -> equalHack "[\"Sum12C\",[[-2,14,5,18,-11,9],[-9,-9,-10,3,9,-4,-13,17,-14,-13,18,18,13,15,0,17,-7,13]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [-2,14,5,18,-11,9] [-9,-9,-10,3,9,-4,-13,17,-14,-13,18,18,13,15,0,17,-7,13]))))
  , test "11" (\_ -> equalHack "[\"Sum12C\",[[1,0,3,15,11,7,-2,-6,-20,-11,-20,12,-14,-17,-17],[-16,19,-11,7,18,12,2,-7,5,13]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [1,0,3,15,11,7,-2,-6,-20,-11,-20,12,-14,-17,-17] [-16,19,-11,7,18,12,2,-7,5,13]))))
  ]

sumDecode01 : Test
sumDecode01 = describe "Sum decode 01"
  [ test "1" (\_ -> equal (Ok (Sum01E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum01A [-1,1])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-1,1]}"))
  , test "3" (\_ -> equal (Ok (Sum01C [-1,2,4,-3] [])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[-1,2,4,-3],[]]}"))
  , test "4" (\_ -> equal (Ok (Sum01D {foo = [6,5,3]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[6,5,3]}"))
  , test "5" (\_ -> equal (Ok (Sum01D {foo = [2,-8,4]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[2,-8,4]}"))
  , test "6" (\_ -> equal (Ok (Sum01B (Just [-2,-8,-6]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[-2,-8,-6]}"))
  , test "7" (\_ -> equal (Ok (Sum01C [-8,1,-10,-1,2] [9,4,5,-12,2,-5,8])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[-8,1,-10,-1,2],[9,4,5,-12,2,-5,8]]}"))
  , test "8" (\_ -> equal (Ok (Sum01A [-14,1,-9,-2,0,-1,7,-14,-14,5,-10])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-14,1,-9,-2,0,-1,7,-14,-14,5,-10]}"))
  , test "9" (\_ -> equal (Ok (Sum01A [13,15,-14,3,0,2,-9,4,-1,-2,-5,-2,3,-15,5])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[13,15,-14,3,0,2,-9,4,-1,-2,-5,-2,3,-15,5]}"))
  , test "10" (\_ -> equal (Ok (Sum01E {bar = -17, baz = 13})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":-17,\"baz\":13}"))
  , test "11" (\_ -> equal (Ok (Sum01C [19,5,5,3,-12,-9,14,18,-18,-2,-15,-11,-16,13,-20] [-13,-3,-13,-15,-2])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[19,5,5,3,-12,-9,14,18,-18,-2,-15,-11,-16,13,-20],[-13,-3,-13,-15,-2]]}"))
  ]

sumDecode02 : Test
sumDecode02 = describe "Sum decode 02"
  [ test "1" (\_ -> equal (Ok (Sum02E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum02D {foo = [-2,2]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[-2,2]}"))
  , test "3" (\_ -> equal (Ok (Sum02A [3,-2])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[3,-2]}"))
  , test "4" (\_ -> equal (Ok (Sum02A [-3,3,4])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[-3,3,4]}"))
  , test "5" (\_ -> equal (Ok (Sum02A [-2,-4,-1,-1,5,-7])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[-2,-4,-1,-1,5,-7]}"))
  , test "6" (\_ -> equal (Ok (Sum02E {bar = -1, baz = 10})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-1,\"baz\":10}"))
  , test "7" (\_ -> equal (Ok (Sum02E {bar = 8, baz = 12})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":8,\"baz\":12}"))
  , test "8" (\_ -> equal (Ok (Sum02E {bar = -9, baz = -13})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-9,\"baz\":-13}"))
  , test "9" (\_ -> equal (Ok (Sum02C [-9] [])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[-9],[]]}"))
  , test "10" (\_ -> equal (Ok (Sum02A [14,4,5,-2,-2])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[14,4,5,-2,-2]}"))
  , test "11" (\_ -> equal (Ok (Sum02D {foo = [19,17,14,14,1,-20,-4,9,2,3,2,4,16,-19,-15]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[19,17,14,14,1,-20,-4,9,2,3,2,4,16,-19,-15]}"))
  ]

sumDecode03 : Test
sumDecode03 = describe "Sum decode 03"
  [ test "1" (\_ -> equal (Ok (Sum03C [] [])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum03C [-1] [])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-1],[]]}"))
  , test "3" (\_ -> equal (Ok (Sum03C [-2,3,1,-4] [1,-1,1])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-2,3,1,-4],[1,-1,1]]}"))
  , test "4" (\_ -> equal (Ok (Sum03E {bar = 3, baz = -4})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":3,\"baz\":-4}"))
  , test "5" (\_ -> equal (Ok (Sum03D {foo = [-3,-1,5,-4]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-3,-1,5,-4]}"))
  , test "6" (\_ -> equal (Ok (Sum03B (Just [10,-3]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[10,-3]}"))
  , test "7" (\_ -> equal (Ok (Sum03C [-4,-5,-8] [-5])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-4,-5,-8],[-5]]}"))
  , test "8" (\_ -> equal (Ok (Sum03A [5,5,1,2,0,5,-12,13,-2,-3,-4,11,4,12])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[5,5,1,2,0,5,-12,13,-2,-3,-4,11,4,12]}"))
  , test "9" (\_ -> equal (Ok (Sum03A [])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[]}"))
  , test "10" (\_ -> equal (Ok (Sum03D {foo = [-16,-6,10,16,-9,-15,6,14,-12,9,1,-6,17,12,-12]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-16,-6,10,16,-9,-15,6,14,-12,9,1,-6,17,12,-12]}"))
  , test "11" (\_ -> equal (Ok (Sum03C [11,19,20,-8,12,19,-14,10,2,-14,1,4,-9] [14,-12,-12,2,-11,7,14,17,-1,8,7,2,-18,6,4,13,13,5])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[11,19,20,-8,12,19,-14,10,2,-14,1,4,-9],[14,-12,-12,2,-11,7,14,17,-1,8,7,2,-18,6,4,13,13,5]]}"))
  ]

sumDecode04 : Test
sumDecode04 = describe "Sum decode 04"
  [ test "1" (\_ -> equal (Ok (Sum04D {foo = []})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum04C [-2,1] [])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[-2,1],[]]}"))
  , test "3" (\_ -> equal (Ok (Sum04B (Just [-3,-1,1]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-3,-1,1]}"))
  , test "4" (\_ -> equal (Ok (Sum04E {bar = -5, baz = 4})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":-5,\"baz\":4}"))
  , test "5" (\_ -> equal (Ok (Sum04C [0,-8,-5,-5,6] [-2,8,-3])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[0,-8,-5,-5,6],[-2,8,-3]]}"))
  , test "6" (\_ -> equal (Ok (Sum04A [8,-4,2,8,-2,9,4,6,4,1])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[8,-4,2,8,-2,9,4,6,4,1]}"))
  , test "7" (\_ -> equal (Ok (Sum04C [0,4,11,-9,-4,9,2,9,7,7,8] [-4,-2,12,9,-3,7,-6,7,-12,8])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[0,4,11,-9,-4,9,2,9,7,7,8],[-4,-2,12,9,-3,7,-6,7,-12,8]]}"))
  , test "8" (\_ -> equal (Ok (Sum04C [] [6,-1,-9,-4,14,6,-6,9,8,1,7,13])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[6,-1,-9,-4,14,6,-6,9,8,1,7,13]]}"))
  , test "9" (\_ -> equal (Ok (Sum04D {foo = [2,6,2,6,13,-4,11]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[2,6,2,6,13,-4,11]}"))
  , test "10" (\_ -> equal (Ok (Sum04E {bar = 4, baz = -10})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":4,\"baz\":-10}"))
  , test "11" (\_ -> equal (Ok (Sum04D {foo = [-18,15,-10,-20,-18,7,-5,12,-11,19,17,3,14,8,-6,1,-5,-15]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[-18,15,-10,-20,-18,7,-5,12,-11,19,17,3,14,8,-6,1,-5,-15]}"))
  ]

sumDecode05 : Test
sumDecode05 = describe "Sum decode 05"
  [ test "1" (\_ -> equal (Ok (Sum05B (Just []))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum05C [0] [-2,0])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[0],[-2,0]]}"))
  , test "3" (\_ -> equal (Ok (Sum05A [3,-2,4,0])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[3,-2,4,0]}"))
  , test "4" (\_ -> equal (Ok (Sum05B (Just [4,-5,3,6,2]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[4,-5,3,6,2]}"))
  , test "5" (\_ -> equal (Ok (Sum05D {foo = [5,8,-5,-8,8,3,4]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[5,8,-5,-8,8,3,4]}}"))
  , test "6" (\_ -> equal (Ok (Sum05C [-2,3] [-8,8])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[-2,3],[-8,8]]}"))
  , test "7" (\_ -> equal (Ok (Sum05A [-10,-7])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[-10,-7]}"))
  , test "8" (\_ -> equal (Ok (Sum05A [-8,8,7,-10])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[-8,8,7,-10]}"))
  , test "9" (\_ -> equal (Ok (Sum05D {foo = [2,-1,1,-6,-8,-1,1,-2,16,4,7,-1,-13]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[2,-1,1,-6,-8,-1,1,-2,16,4,7,-1,-13]}}"))
  , test "10" (\_ -> equal (Ok (Sum05C [-4,3,-2,3,1,17,16,4,-2,-5,-10] [11,18,-8,7,3])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[-4,3,-2,3,1,17,16,4,-2,-5,-10],[11,18,-8,7,3]]}"))
  , test "11" (\_ -> equal (Ok (Sum05E {bar = 18, baz = 8})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":18,\"baz\":8}}"))
  ]

sumDecode06 : Test
sumDecode06 = describe "Sum decode 06"
  [ test "1" (\_ -> equal (Ok (Sum06D {foo = []})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum06E {bar = 1, baz = 2})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":1,\"baz\":2}}"))
  , test "3" (\_ -> equal (Ok (Sum06B (Just [-1]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[-1]}"))
  , test "4" (\_ -> equal (Ok (Sum06B (Just [5,5,3]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[5,5,3]}"))
  , test "5" (\_ -> equal (Ok (Sum06D {foo = [5,-4]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[5,-4]}}"))
  , test "6" (\_ -> equal (Ok (Sum06B (Just [3,3]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[3,3]}"))
  , test "7" (\_ -> equal (Ok (Sum06A [-12,-3,0,-7,-4,0,-9,0])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[-12,-3,0,-7,-4,0,-9,0]}"))
  , test "8" (\_ -> equal (Ok (Sum06D {foo = [0,0,-6,7,-14,-12,-10,1,3,-14,-6,-10]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[0,0,-6,7,-14,-12,-10,1,3,-14,-6,-10]}}"))
  , test "9" (\_ -> equal (Ok (Sum06B (Just [14,6,5,-14,5,-4,16,3,-11,1,-5,-7,12,4,-1,-6]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[14,6,5,-14,5,-4,16,3,-11,1,-5,-7,12,4,-1,-6]}"))
  , test "10" (\_ -> equal (Ok (Sum06C [16,7,17,18,-15,15,-15,-4,1,-10,-8,11,-7,-8] [11,9,-13,-3,-17,12])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[16,7,17,18,-15,15,-15,-4,1,-10,-8,11,-7,-8],[11,9,-13,-3,-17,12]]}"))
  , test "11" (\_ -> equal (Ok (Sum06C [-14,20,-11,18,5,1,-12,-8,-15,-9,-5] [0,7])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[-14,20,-11,18,5,1,-12,-8,-15,-9,-5],[0,7]]}"))
  ]

sumDecode07 : Test
sumDecode07 = describe "Sum decode 07"
  [ test "1" (\_ -> equal (Ok (Sum07A [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum07E {bar = -2, baz = 2})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":-2,\"baz\":2}}"))
  , test "3" (\_ -> equal (Ok (Sum07D {foo = [-4,-2]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[-4,-2]}}"))
  , test "4" (\_ -> equal (Ok (Sum07C [] [1])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[],[1]]}"))
  , test "5" (\_ -> equal (Ok (Sum07D {foo = [-6,7]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[-6,7]}}"))
  , test "6" (\_ -> equal (Ok (Sum07A [9,8,-6,-3,-8])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[9,8,-6,-3,-8]}"))
  , test "7" (\_ -> equal (Ok (Sum07C [-12,-4,-12,0,10,0,7] [-7,-3,7,-8,6,-9,1,4])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-12,-4,-12,0,10,0,7],[-7,-3,7,-8,6,-9,1,4]]}"))
  , test "8" (\_ -> equal (Ok (Sum07C [8,9,-7,2,12,12,-9,0,7,-6,4] [-11,11,-9,-11,-2,-10,-9,-14,-3])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[8,9,-7,2,12,12,-9,0,7,-6,4],[-11,11,-9,-11,-2,-10,-9,-14,-3]]}"))
  , test "9" (\_ -> equal (Ok (Sum07A [-12,12,-1,-16])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[-12,12,-1,-16]}"))
  , test "10" (\_ -> equal (Ok (Sum07D {foo = [15,2,6,14,1,-17,-7,6,8]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[15,2,6,14,1,-17,-7,6,8]}}"))
  , test "11" (\_ -> equal (Ok (Sum07B (Just [-10,14,-19,-8,-2,17]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-10,14,-19,-8,-2,17]}"))
  ]

sumDecode08 : Test
sumDecode08 = describe "Sum decode 08"
  [ test "1" (\_ -> equal (Ok (Sum08E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":0,\"baz\":0}}"))
  , test "2" (\_ -> equal (Ok (Sum08E {bar = -2, baz = 0})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":-2,\"baz\":0}}"))
  , test "3" (\_ -> equal (Ok (Sum08D {foo = [-3,-4]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[-3,-4]}}"))
  , test "4" (\_ -> equal (Ok (Sum08D {foo = [5,-3]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[5,-3]}}"))
  , test "5" (\_ -> equal (Ok (Sum08E {bar = 3, baz = -1})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":3,\"baz\":-1}}"))
  , test "6" (\_ -> equal (Ok (Sum08A [6,-9,5,6,3,-2])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[6,-9,5,6,3,-2]}"))
  , test "7" (\_ -> equal (Ok (Sum08C [-5,9,12] [8,6,-3])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[-5,9,12],[8,6,-3]]}"))
  , test "8" (\_ -> equal (Ok (Sum08C [] [2,1,-12,3,7,2])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[],[2,1,-12,3,7,2]]}"))
  , test "9" (\_ -> equal (Ok (Sum08A [-5,-2,-14,14,2,-9,-15,-10,5])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[-5,-2,-14,14,2,-9,-15,-10,5]}"))
  , test "10" (\_ -> equal (Ok (Sum08C [14,-5,14,18,4,14,-6,-1,-11,-10] [-17,0,12,10,-5,-18,17])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[14,-5,14,18,4,14,-6,-1,-11,-10],[-17,0,12,10,-5,-18,17]]}"))
  , test "11" (\_ -> equal (Ok (Sum08C [-14,-9,-18,2] [18,-12,0])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[-14,-9,-18,2],[18,-12,0]]}"))
  ]

sumDecode09 : Test
sumDecode09 = describe "Sum decode 09"
  [ test "1" (\_ -> equal (Ok (Sum09E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":0,\"baz\":0}]"))
  , test "2" (\_ -> equal (Ok (Sum09C [2] [])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[2],[]]]"))
  , test "3" (\_ -> equal (Ok (Sum09E {bar = -3, baz = -2})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-3,\"baz\":-2}]"))
  , test "4" (\_ -> equal (Ok (Sum09C [-2,1,-2,-3,2] [1,-2,0,-2])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[-2,1,-2,-3,2],[1,-2,0,-2]]]"))
  , test "5" (\_ -> equal (Ok (Sum09A [3,-7,-2,0,3])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[3,-7,-2,0,3]]"))
  , test "6" (\_ -> equal (Ok (Sum09D {foo = [-2,5,-9,7,0,10,7,10,-1]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[-2,5,-9,7,0,10,7,10,-1]}]"))
  , test "7" (\_ -> equal (Ok (Sum09C [-10,-7,-7,-11,-9,8,-6] [12])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[-10,-7,-7,-11,-9,8,-6],[12]]]"))
  , test "8" (\_ -> equal (Ok (Sum09B (Just [8,10,-5,7,-3,-2,-8,-1,-11,14]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[8,10,-5,7,-3,-2,-8,-1,-11,14]]"))
  , test "9" (\_ -> equal (Ok (Sum09E {bar = -12, baz = 3})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-12,\"baz\":3}]"))
  , test "10" (\_ -> equal (Ok (Sum09B (Just [14,5,-18,-7,-18,-16,2,8,4,7,-8,5,10,10]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[14,5,-18,-7,-18,-16,2,8,4,7,-8,5,10,10]]"))
  , test "11" (\_ -> equal (Ok (Sum09E {bar = -15, baz = 18})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-15,\"baz\":18}]"))
  ]

sumDecode10 : Test
sumDecode10 = describe "Sum decode 10"
  [ test "1" (\_ -> equal (Ok (Sum10C [] [])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[],[]]]"))
  , test "2" (\_ -> equal (Ok (Sum10D {foo = [1]})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[1]}]"))
  , test "3" (\_ -> equal (Ok (Sum10E {bar = 0, baz = -2})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":0,\"baz\":-2}]"))
  , test "4" (\_ -> equal (Ok (Sum10E {bar = -1, baz = 2})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":-1,\"baz\":2}]"))
  , test "5" (\_ -> equal (Ok (Sum10D {foo = [5,-3,7,0,-2,-4]})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[5,-3,7,0,-2,-4]}]"))
  , test "6" (\_ -> equal (Ok (Sum10D {foo = [-8,-6,7,0,5]})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[-8,-6,7,0,5]}]"))
  , test "7" (\_ -> equal (Ok (Sum10B (Just [-1,-7,0,6,7,2,-12,12,-8,-12]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-1,-7,0,6,7,2,-12,12,-8,-12]]"))
  , test "8" (\_ -> equal (Ok (Sum10A [-12,0,6,12,-14,14,-6,8,10,14,7])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[-12,0,6,12,-14,14,-6,8,10,14,7]]"))
  , test "9" (\_ -> equal (Ok (Sum10A [])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[]]"))
  , test "10" (\_ -> equal (Ok (Sum10E {bar = 16, baz = 14})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":16,\"baz\":14}]"))
  , test "11" (\_ -> equal (Ok (Sum10A [10,-14,6,9,-15,7,5,13,-1,10,-10,-6,-6,5])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[10,-14,6,9,-15,7,5,13,-1,10,-10,-6,-6,5]]"))
  ]

sumDecode11 : Test
sumDecode11 = describe "Sum decode 11"
  [ test "1" (\_ -> equal (Ok (Sum11C [] [])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[],[]]]"))
  , test "2" (\_ -> equal (Ok (Sum11A [1,0])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[1,0]]"))
  , test "3" (\_ -> equal (Ok (Sum11B (Just [3,3,2]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[3,3,2]]"))
  , test "4" (\_ -> equal (Ok (Sum11E {bar = 4, baz = -1})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":4,\"baz\":-1}]"))
  , test "5" (\_ -> equal (Ok (Sum11A [3,1,1,3,-5,4,5,-7])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[3,1,1,3,-5,4,5,-7]]"))
  , test "6" (\_ -> equal (Ok (Sum11D {foo = [2,-8,1,-5,-1,-10]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[2,-8,1,-5,-1,-10]}]"))
  , test "7" (\_ -> equal (Ok (Sum11E {bar = 0, baz = 12})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":0,\"baz\":12}]"))
  , test "8" (\_ -> equal (Ok (Sum11E {bar = -10, baz = -6})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":-10,\"baz\":-6}]"))
  , test "9" (\_ -> equal (Ok (Sum11A [4])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[4]]"))
  , test "10" (\_ -> equal (Ok (Sum11C [] [16,8,-7,16,18,-6,7,1,-8,15,-6,1,-1])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[],[16,8,-7,16,18,-6,7,1,-8,15,-6,1,-1]]]"))
  , test "11" (\_ -> equal (Ok (Sum11D {foo = [-16,6,14,2,-11,8,13,13,-13,-15]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[-16,6,14,2,-11,8,13,13,-13,-15]}]"))
  ]

sumDecode12 : Test
sumDecode12 = describe "Sum decode 12"
  [ test "1" (\_ -> equal (Ok (Sum12B (Just []))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum12D {foo = []})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[]}]"))
  , test "3" (\_ -> equal (Ok (Sum12B (Just [-1]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[-1]]"))
  , test "4" (\_ -> equal (Ok (Sum12D {foo = [6,5,0]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[6,5,0]}]"))
  , test "5" (\_ -> equal (Ok (Sum12E {bar = 4, baz = 0})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":4,\"baz\":0}]"))
  , test "6" (\_ -> equal (Ok (Sum12A [-5,4,-7,-9,-4,-8,-9,8,6,-5])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-5,4,-7,-9,-4,-8,-9,8,6,-5]]"))
  , test "7" (\_ -> equal (Ok (Sum12E {bar = -7, baz = -7})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":-7,\"baz\":-7}]"))
  , test "8" (\_ -> equal (Ok (Sum12C [4,-2,1,-6,14,3,-2,-14,9] [-1,-6,-13,-7,2,-4,-6,-13,-10,-5,14,3,6])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[4,-2,1,-6,14,3,-2,-14,9],[-1,-6,-13,-7,2,-4,-6,-13,-10,-5,14,3,6]]]"))
  , test "9" (\_ -> equal (Ok (Sum12B (Just [-11,-12,15,7,-12,9,-6,9]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[-11,-12,15,7,-12,9,-6,9]]"))
  , test "10" (\_ -> equal (Ok (Sum12C [-2,14,5,18,-11,9] [-9,-9,-10,3,9,-4,-13,17,-14,-13,18,18,13,15,0,17,-7,13])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[-2,14,5,18,-11,9],[-9,-9,-10,3,9,-4,-13,17,-14,-13,18,18,13,15,0,17,-7,13]]]"))
  , test "11" (\_ -> equal (Ok (Sum12C [1,0,3,15,11,7,-2,-6,-20,-11,-20,12,-14,-17,-17] [-16,19,-11,7,18,12,2,-7,5,13])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[1,0,3,15,11,7,-2,-6,-20,-11,-20,12,-14,-17,-17],[-16,19,-11,7,18,12,2,-7,5,13]]]"))
  ]

recordDecode1 : Test
recordDecode1 = describe "Record decode 1"
  [ test "1" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"))
  , test "2" (\_ -> equal (Ok (Record1 {foo = -2, bar = Just (-1), baz = [-2], qux = Just [2,2], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-2,\"bar\":-1,\"baz\":[-2],\"qux\":[2,2],\"jmap\":{\"a\":-1}}"))
  , test "3" (\_ -> equal (Ok (Record1 {foo = -4, bar = Just 3, baz = [-1,-4,4,-4], qux = Just [-4,0,4], jmap = fromList [("a",4)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-4,\"bar\":3,\"baz\":[-1,-4,4,-4],\"qux\":[-4,0,4],\"jmap\":{\"a\":4}}"))
  , test "4" (\_ -> equal (Ok (Record1 {foo = -6, bar = Just 3, baz = [6,-1,-2,1,1,4], qux = Just [-3,1,-1,6,5], jmap = fromList [("a",1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-6,\"bar\":3,\"baz\":[6,-1,-2,1,1,4],\"qux\":[-3,1,-1,6,5],\"jmap\":{\"a\":1}}"))
  , test "5" (\_ -> equal (Ok (Record1 {foo = 5, bar = Just 7, baz = [5,2,-4,-2], qux = Just [], jmap = fromList [("a",-8)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":5,\"bar\":7,\"baz\":[5,2,-4,-2],\"qux\":[],\"jmap\":{\"a\":-8}}"))
  , test "6" (\_ -> equal (Ok (Record1 {foo = -9, bar = Just (-7), baz = [10,7,-4,4,2,-1], qux = Just [0,-5,-2,0,0,10,-3,10], jmap = fromList [("a",6)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-9,\"bar\":-7,\"baz\":[10,7,-4,4,2,-1],\"qux\":[0,-5,-2,0,0,10,-3,10],\"jmap\":{\"a\":6}}"))
  , test "7" (\_ -> equal (Ok (Record1 {foo = 2, bar = Just 6, baz = [], qux = Just [1,-10,8,-10,11,-12], jmap = fromList [("a",7)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":2,\"bar\":6,\"baz\":[],\"qux\":[1,-10,8,-10,11,-12],\"jmap\":{\"a\":7}}"))
  , test "8" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 1, baz = [-11,14], qux = Just [-14,14,-3,-2], jmap = fromList [("a",-7)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":1,\"baz\":[-11,14],\"qux\":[-14,14,-3,-2],\"jmap\":{\"a\":-7}}"))
  , test "9" (\_ -> equal (Ok (Record1 {foo = -9, bar = Just 10, baz = [10,1,16,7,0,11,10,1,-16,-2], qux = Just [0,-1,-1,13,15,-12,-13,3,5,0], jmap = fromList [("a",9)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-9,\"bar\":10,\"baz\":[10,1,16,7,0,11,10,1,-16,-2],\"qux\":[0,-1,-1,13,15,-12,-13,3,5,0],\"jmap\":{\"a\":9}}"))
  , test "10" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just (-10), baz = [16,-12,-14,9,-14,4], qux = Just [], jmap = fromList [("a",-8)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":-10,\"baz\":[16,-12,-14,9,-14,4],\"qux\":[],\"jmap\":{\"a\":-8}}"))
  , test "11" (\_ -> equal (Ok (Record1 {foo = -2, bar = Just 14, baz = [-20,-10,10,-1,-3,-3,-19,-2,-8,-7,-5,20,11,17,-19,-7], qux = Just [10,6,16,16,1,-20,18], jmap = fromList [("a",16)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-2,\"bar\":14,\"baz\":[-20,-10,10,-1,-3,-3,-19,-2,-8,-7,-5,20,11,17,-19,-7],\"qux\":[10,6,16,16,1,-20,18],\"jmap\":{\"a\":16}}"))
  ]

recordDecode2 : Test
recordDecode2 = describe "Record decode 2"
  [ test "1" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"))
  , test "2" (\_ -> equal (Ok (Record2 {foo = -1, bar = Just (-1), baz = [0], qux = Just [1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-1,\"qux\":[1],\"foo\":-1,\"baz\":[0]}"))
  , test "3" (\_ -> equal (Ok (Record2 {foo = -4, bar = Just 2, baz = [1,-3], qux = Just [-3]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":2,\"qux\":[-3],\"foo\":-4,\"baz\":[1,-3]}"))
  , test "4" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just 6, baz = [-3,2,5,-3,3,2], qux = Just [-2,-1,6,-5,-3,-2]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":6,\"qux\":[-2,-1,6,-5,-3,-2],\"foo\":1,\"baz\":[-3,2,5,-3,3,2]}"))
  , test "5" (\_ -> equal (Ok (Record2 {foo = 6, bar = Just (-8), baz = [4], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-8,\"qux\":[],\"foo\":6,\"baz\":[4]}"))
  , test "6" (\_ -> equal (Ok (Record2 {foo = -8, bar = Just 0, baz = [0,-7,-9,-2,1,10,-7,10,2,9], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[],\"foo\":-8,\"baz\":[0,-7,-9,-2,1,10,-7,10,2,9]}"))
  , test "7" (\_ -> equal (Ok (Record2 {foo = 2, bar = Just 0, baz = [10,-12,-8,-7,-10,-4,-9,-4,-10,-9], qux = Just [1,10,-10]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[1,10,-10],\"foo\":2,\"baz\":[10,-12,-8,-7,-10,-4,-9,-4,-10,-9]}"))
  , test "8" (\_ -> equal (Ok (Record2 {foo = 10, bar = Just 5, baz = [], qux = Just [-13,10,13,13,-7,6,10]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":5,\"qux\":[-13,10,13,13,-7,6,10],\"foo\":10,\"baz\":[]}"))
  , test "9" (\_ -> equal (Ok (Record2 {foo = 8, bar = Just (-11), baz = [-2,-3,-11,7,12,3,2,15,4,14,9,-2,-10,-5], qux = Just [7,-13,7,-6,13,4]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-11,\"qux\":[7,-13,7,-6,13,4],\"foo\":8,\"baz\":[-2,-3,-11,7,12,3,2,15,4,14,9,-2,-10,-5]}"))
  , test "10" (\_ -> equal (Ok (Record2 {foo = 7, bar = Just (-13), baz = [3,-11,-18,-4,5,18,-15,17,9], qux = Just [-17,-17,-14,7,-8,13,8,-4,15,-2,8,7]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-13,\"qux\":[-17,-17,-14,7,-8,13,8,-4,15,-2,8,7],\"foo\":7,\"baz\":[3,-11,-18,-4,5,18,-15,17,9]}"))
  , test "11" (\_ -> equal (Ok (Record2 {foo = -12, bar = Just 3, baz = [-14,10,-11,3,-9,2,10,-1], qux = Just [10,6,-11,12,-18,-6,-17,17,16,19,9,-12,12,1,18,-9,1,-4]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":3,\"qux\":[10,6,-11,12,-18,-6,-17,17,16,19,9,-12,12,1,18,-9,1,-4],\"foo\":-12,\"baz\":[-14,10,-11,3,-9,2,10,-1]}"))
  ]

recordEncode1 : Test
recordEncode1 = describe "Record encode 1"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]}))))
  , test "2" (\_ -> equalHack "{\"foo\":-2,\"bar\":-1,\"baz\":[-2],\"qux\":[2,2],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -2, bar = Just (-1), baz = [-2], qux = Just [2,2], jmap = fromList [("a",-1)]}))))
  , test "3" (\_ -> equalHack "{\"foo\":-4,\"bar\":3,\"baz\":[-1,-4,4,-4],\"qux\":[-4,0,4],\"jmap\":{\"a\":4}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -4, bar = Just 3, baz = [-1,-4,4,-4], qux = Just [-4,0,4], jmap = fromList [("a",4)]}))))
  , test "4" (\_ -> equalHack "{\"foo\":-6,\"bar\":3,\"baz\":[6,-1,-2,1,1,4],\"qux\":[-3,1,-1,6,5],\"jmap\":{\"a\":1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -6, bar = Just 3, baz = [6,-1,-2,1,1,4], qux = Just [-3,1,-1,6,5], jmap = fromList [("a",1)]}))))
  , test "5" (\_ -> equalHack "{\"foo\":5,\"bar\":7,\"baz\":[5,2,-4,-2],\"qux\":[],\"jmap\":{\"a\":-8}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 5, bar = Just 7, baz = [5,2,-4,-2], qux = Just [], jmap = fromList [("a",-8)]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-9,\"bar\":-7,\"baz\":[10,7,-4,4,2,-1],\"qux\":[0,-5,-2,0,0,10,-3,10],\"jmap\":{\"a\":6}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -9, bar = Just (-7), baz = [10,7,-4,4,2,-1], qux = Just [0,-5,-2,0,0,10,-3,10], jmap = fromList [("a",6)]}))))
  , test "7" (\_ -> equalHack "{\"foo\":2,\"bar\":6,\"baz\":[],\"qux\":[1,-10,8,-10,11,-12],\"jmap\":{\"a\":7}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 2, bar = Just 6, baz = [], qux = Just [1,-10,8,-10,11,-12], jmap = fromList [("a",7)]}))))
  , test "8" (\_ -> equalHack "{\"foo\":0,\"bar\":1,\"baz\":[-11,14],\"qux\":[-14,14,-3,-2],\"jmap\":{\"a\":-7}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 1, baz = [-11,14], qux = Just [-14,14,-3,-2], jmap = fromList [("a",-7)]}))))
  , test "9" (\_ -> equalHack "{\"foo\":-9,\"bar\":10,\"baz\":[10,1,16,7,0,11,10,1,-16,-2],\"qux\":[0,-1,-1,13,15,-12,-13,3,5,0],\"jmap\":{\"a\":9}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -9, bar = Just 10, baz = [10,1,16,7,0,11,10,1,-16,-2], qux = Just [0,-1,-1,13,15,-12,-13,3,5,0], jmap = fromList [("a",9)]}))))
  , test "10" (\_ -> equalHack "{\"foo\":-3,\"bar\":-10,\"baz\":[16,-12,-14,9,-14,4],\"qux\":[],\"jmap\":{\"a\":-8}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -3, bar = Just (-10), baz = [16,-12,-14,9,-14,4], qux = Just [], jmap = fromList [("a",-8)]}))))
  , test "11" (\_ -> equalHack "{\"foo\":-2,\"bar\":14,\"baz\":[-20,-10,10,-1,-3,-3,-19,-2,-8,-7,-5,20,11,17,-19,-7],\"qux\":[10,6,16,16,1,-20,18],\"jmap\":{\"a\":16}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -2, bar = Just 14, baz = [-20,-10,10,-1,-3,-3,-19,-2,-8,-7,-5,20,11,17,-19,-7], qux = Just [10,6,16,16,1,-20,18], jmap = fromList [("a",16)]}))))
  ]

recordEncode2 : Test
recordEncode2 = describe "Record encode 2"
  [ test "1" (\_ -> equalHack "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"bar\":-1,\"qux\":[1],\"foo\":-1,\"baz\":[0]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -1, bar = Just (-1), baz = [0], qux = Just [1]}))))
  , test "3" (\_ -> equalHack "{\"bar\":2,\"qux\":[-3],\"foo\":-4,\"baz\":[1,-3]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -4, bar = Just 2, baz = [1,-3], qux = Just [-3]}))))
  , test "4" (\_ -> equalHack "{\"bar\":6,\"qux\":[-2,-1,6,-5,-3,-2],\"foo\":1,\"baz\":[-3,2,5,-3,3,2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just 6, baz = [-3,2,5,-3,3,2], qux = Just [-2,-1,6,-5,-3,-2]}))))
  , test "5" (\_ -> equalHack "{\"bar\":-8,\"qux\":[],\"foo\":6,\"baz\":[4]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 6, bar = Just (-8), baz = [4], qux = Just []}))))
  , test "6" (\_ -> equalHack "{\"bar\":0,\"qux\":[],\"foo\":-8,\"baz\":[0,-7,-9,-2,1,10,-7,10,2,9]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -8, bar = Just 0, baz = [0,-7,-9,-2,1,10,-7,10,2,9], qux = Just []}))))
  , test "7" (\_ -> equalHack "{\"bar\":0,\"qux\":[1,10,-10],\"foo\":2,\"baz\":[10,-12,-8,-7,-10,-4,-9,-4,-10,-9]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 2, bar = Just 0, baz = [10,-12,-8,-7,-10,-4,-9,-4,-10,-9], qux = Just [1,10,-10]}))))
  , test "8" (\_ -> equalHack "{\"bar\":5,\"qux\":[-13,10,13,13,-7,6,10],\"foo\":10,\"baz\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 10, bar = Just 5, baz = [], qux = Just [-13,10,13,13,-7,6,10]}))))
  , test "9" (\_ -> equalHack "{\"bar\":-11,\"qux\":[7,-13,7,-6,13,4],\"foo\":8,\"baz\":[-2,-3,-11,7,12,3,2,15,4,14,9,-2,-10,-5]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 8, bar = Just (-11), baz = [-2,-3,-11,7,12,3,2,15,4,14,9,-2,-10,-5], qux = Just [7,-13,7,-6,13,4]}))))
  , test "10" (\_ -> equalHack "{\"bar\":-13,\"qux\":[-17,-17,-14,7,-8,13,8,-4,15,-2,8,7],\"foo\":7,\"baz\":[3,-11,-18,-4,5,18,-15,17,9]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 7, bar = Just (-13), baz = [3,-11,-18,-4,5,18,-15,17,9], qux = Just [-17,-17,-14,7,-8,13,8,-4,15,-2,8,7]}))))
  , test "11" (\_ -> equalHack "{\"bar\":3,\"qux\":[10,6,-11,12,-18,-6,-17,17,16,19,9,-12,12,1,18,-9,1,-4],\"foo\":-12,\"baz\":[-14,10,-11,3,-9,2,10,-1]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -12, bar = Just 3, baz = [-14,10,-11,3,-9,2,10,-1], qux = Just [10,6,-11,12,-18,-6,-17,17,16,19,9,-12,12,1,18,-9,1,-4]}))))
  ]

simpleEncode01 : Test
simpleEncode01 = describe "Simple encode 01"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 []))))
  , test "2" (\_ -> equalHack "[2,1]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [2,1]))))
  , test "3" (\_ -> equalHack "[3,4,4,-2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [3,4,4,-2]))))
  , test "4" (\_ -> equalHack "[1,0,6]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [1,0,6]))))
  , test "5" (\_ -> equalHack "[4,5,0,-8,3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [4,5,0,-8,3]))))
  , test "6" (\_ -> equalHack "[8,-3,9,-4,6,9,7,4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [8,-3,9,-4,6,9,7,4]))))
  , test "7" (\_ -> equalHack "[-5,-7]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-5,-7]))))
  , test "8" (\_ -> equalHack "[-10,-3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-10,-3]))))
  , test "9" (\_ -> equalHack "[-8,-15,-4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-8,-15,-4]))))
  , test "10" (\_ -> equalHack "[-16,-18,1,8,2,-12,-2,-5,-13,14,3,-15,9]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-16,-18,1,8,2,-12,-2,-5,-13,14,3,-15,9]))))
  , test "11" (\_ -> equalHack "[-5,-1,-8,-6,6,8]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-5,-1,-8,-6,6,8]))))
  ]

simpleEncode02 : Test
simpleEncode02 = describe "Simple encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "2" (\_ -> equalHack "[-2,-2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-2,-2]))))
  , test "3" (\_ -> equalHack "[-2,-1,2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-2,-1,2]))))
  , test "4" (\_ -> equalHack "[-6,0]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-6,0]))))
  , test "5" (\_ -> equalHack "[8,-8,2,1,-5,4,6,3]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [8,-8,2,1,-5,4,6,3]))))
  , test "6" (\_ -> equalHack "[3,0,5,-1,2,-7,3,-4]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [3,0,5,-1,2,-7,3,-4]))))
  , test "7" (\_ -> equalHack "[-7,2,-11,9,-1,8,-9,4,5,-6,9,-12]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-7,2,-11,9,-1,8,-9,4,5,-6,9,-12]))))
  , test "8" (\_ -> equalHack "[-1,-7,-1,-8,-8,13,-8,-5,-9]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,-7,-1,-8,-8,13,-8,-5,-9]))))
  , test "9" (\_ -> equalHack "[-1,-1,13,-5,-7,-9]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,-1,13,-5,-7,-9]))))
  , test "10" (\_ -> equalHack "[3,-13,-15,-11,-13,13,6,-2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [3,-13,-15,-11,-13,13,6,-2]))))
  , test "11" (\_ -> equalHack "[3,9,-12,-5,-12,5,-20,1,-18,17,-9]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [3,9,-12,-5,-12,5,-20,1,-18,17,-9]))))
  ]

simpleEncode03 : Test
simpleEncode03 = describe "Simple encode 03"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "3" (\_ -> equalHack "[1,3,-4]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [1,3,-4]))))
  , test "4" (\_ -> equalHack "[-3,4,0]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-3,4,0]))))
  , test "5" (\_ -> equalHack "[-8,-8,5,-4,6,-7,8]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-8,-8,5,-4,6,-7,8]))))
  , test "6" (\_ -> equalHack "[-9,8,-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-9,8,-1]))))
  , test "7" (\_ -> equalHack "[9,-6,2,11,8,-8,1,3,-7,-3]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [9,-6,2,11,8,-8,1,3,-7,-3]))))
  , test "8" (\_ -> equalHack "[-8,-14,-2,10,6,-1,11,-2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-8,-14,-2,10,6,-1,11,-2]))))
  , test "9" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [2]))))
  , test "10" (\_ -> equalHack "[16,1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [16,1]))))
  , test "11" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  ]

simpleEncode04 : Test
simpleEncode04 = describe "Simple encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "2" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [2]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "4" (\_ -> equalHack "[-6,-4]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-6,-4]))))
  , test "5" (\_ -> equalHack "[2,-5,5,-3]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [2,-5,5,-3]))))
  , test "6" (\_ -> equalHack "[-5,-4]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-5,-4]))))
  , test "7" (\_ -> equalHack "[-12,2,-11,0,5,-7]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-12,2,-11,0,5,-7]))))
  , test "8" (\_ -> equalHack "[1,5,14,4,1,4,-9,6,-12,-14]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [1,5,14,4,1,4,-9,6,-12,-14]))))
  , test "9" (\_ -> equalHack "[2,-2,15,2,0,-14,12,-10,14]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [2,-2,15,2,0,-14,12,-10,14]))))
  , test "10" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "11" (\_ -> equalHack "[-2,16]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-2,16]))))
  ]

simpleDecode01 : Test
simpleDecode01 = describe "Simple decode 01"
  [ test "1" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple01 [2,1])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[2,1]"))
  , test "3" (\_ -> equal (Ok (Simple01 [3,4,4,-2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[3,4,4,-2]"))
  , test "4" (\_ -> equal (Ok (Simple01 [1,0,6])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[1,0,6]"))
  , test "5" (\_ -> equal (Ok (Simple01 [4,5,0,-8,3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[4,5,0,-8,3]"))
  , test "6" (\_ -> equal (Ok (Simple01 [8,-3,9,-4,6,9,7,4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[8,-3,9,-4,6,9,7,4]"))
  , test "7" (\_ -> equal (Ok (Simple01 [-5,-7])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-5,-7]"))
  , test "8" (\_ -> equal (Ok (Simple01 [-10,-3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-10,-3]"))
  , test "9" (\_ -> equal (Ok (Simple01 [-8,-15,-4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-8,-15,-4]"))
  , test "10" (\_ -> equal (Ok (Simple01 [-16,-18,1,8,2,-12,-2,-5,-13,14,3,-15,9])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-16,-18,1,8,2,-12,-2,-5,-13,14,3,-15,9]"))
  , test "11" (\_ -> equal (Ok (Simple01 [-5,-1,-8,-6,6,8])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-5,-1,-8,-6,6,8]"))
  ]

simpleDecode02 : Test
simpleDecode02 = describe "Simple decode 02"
  [ test "1" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple02 [-2,-2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-2,-2]"))
  , test "3" (\_ -> equal (Ok (Simple02 [-2,-1,2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-2,-1,2]"))
  , test "4" (\_ -> equal (Ok (Simple02 [-6,0])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-6,0]"))
  , test "5" (\_ -> equal (Ok (Simple02 [8,-8,2,1,-5,4,6,3])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[8,-8,2,1,-5,4,6,3]"))
  , test "6" (\_ -> equal (Ok (Simple02 [3,0,5,-1,2,-7,3,-4])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[3,0,5,-1,2,-7,3,-4]"))
  , test "7" (\_ -> equal (Ok (Simple02 [-7,2,-11,9,-1,8,-9,4,5,-6,9,-12])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-7,2,-11,9,-1,8,-9,4,5,-6,9,-12]"))
  , test "8" (\_ -> equal (Ok (Simple02 [-1,-7,-1,-8,-8,13,-8,-5,-9])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,-7,-1,-8,-8,13,-8,-5,-9]"))
  , test "9" (\_ -> equal (Ok (Simple02 [-1,-1,13,-5,-7,-9])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,-1,13,-5,-7,-9]"))
  , test "10" (\_ -> equal (Ok (Simple02 [3,-13,-15,-11,-13,13,6,-2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[3,-13,-15,-11,-13,13,6,-2]"))
  , test "11" (\_ -> equal (Ok (Simple02 [3,9,-12,-5,-12,5,-20,1,-18,17,-9])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[3,9,-12,-5,-12,5,-20,1,-18,17,-9]"))
  ]

simpleDecode03 : Test
simpleDecode03 = describe "Simple decode 03"
  [ test "1" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (Simple03 [1,3,-4])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[1,3,-4]"))
  , test "4" (\_ -> equal (Ok (Simple03 [-3,4,0])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-3,4,0]"))
  , test "5" (\_ -> equal (Ok (Simple03 [-8,-8,5,-4,6,-7,8])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-8,-8,5,-4,6,-7,8]"))
  , test "6" (\_ -> equal (Ok (Simple03 [-9,8,-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-9,8,-1]"))
  , test "7" (\_ -> equal (Ok (Simple03 [9,-6,2,11,8,-8,1,3,-7,-3])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[9,-6,2,11,8,-8,1,3,-7,-3]"))
  , test "8" (\_ -> equal (Ok (Simple03 [-8,-14,-2,10,6,-1,11,-2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-8,-14,-2,10,6,-1,11,-2]"))
  , test "9" (\_ -> equal (Ok (Simple03 [2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "10" (\_ -> equal (Ok (Simple03 [16,1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[16,1]"))
  , test "11" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  ]

simpleDecode04 : Test
simpleDecode04 = describe "Simple decode 04"
  [ test "1" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple04 [2])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "3" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (Simple04 [-6,-4])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-6,-4]"))
  , test "5" (\_ -> equal (Ok (Simple04 [2,-5,5,-3])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[2,-5,5,-3]"))
  , test "6" (\_ -> equal (Ok (Simple04 [-5,-4])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-5,-4]"))
  , test "7" (\_ -> equal (Ok (Simple04 [-12,2,-11,0,5,-7])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-12,2,-11,0,5,-7]"))
  , test "8" (\_ -> equal (Ok (Simple04 [1,5,14,4,1,4,-9,6,-12,-14])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[1,5,14,4,1,4,-9,6,-12,-14]"))
  , test "9" (\_ -> equal (Ok (Simple04 [2,-2,15,2,0,-14,12,-10,14])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[2,-2,15,2,0,-14,12,-10,14]"))
  , test "10" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "11" (\_ -> equal (Ok (Simple04 [-2,16])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-2,16]"))
  ]

simplerecordEncode01 : Test
simplerecordEncode01 = describe "SimpleRecord encode 01"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "3" (\_ -> equalHack "{\"qux\":[2,0,-2,3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [2,0,-2,3]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [2]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[1,6,5,7,-2,8,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [1,6,5,7,-2,8,4]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[4,10,0,-2,-10,-2,3,0,-9]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [4,10,0,-2,-10,-2,3,0,-9]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[-7,-3,5,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-7,-3,5,4]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[-9,4,-1,-12,2,-8,11,-12,7,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-9,4,-1,-12,2,-8,11,-12,7,-5]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[-6,-9,3,-8,6]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-6,-9,3,-8,6]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[9,-15,-8,15,-7,-1,14,-7,0,14,-8,-7]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [9,-15,-8,15,-7,-1,14,-7,0,14,-8,-7]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[-11,-6,-12,-19,11,7,-1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-11,-6,-12,-19,11,7,-1]}))))
  ]

simplerecordEncode02 : Test
simplerecordEncode02 = describe "SimpleRecord encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "2" (\_ -> equalHack "[-1,2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-1,2]}))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "4" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "5" (\_ -> equalHack "[-1,-5,-7,4,-5,0,-2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-1,-5,-7,4,-5,0,-2]}))))
  , test "6" (\_ -> equalHack "[-5]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-5]}))))
  , test "7" (\_ -> equalHack "[8,-6,-11,4,5,9,0,-12,-6,3,11]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [8,-6,-11,4,5,9,0,-12,-6,3,11]}))))
  , test "8" (\_ -> equalHack "[9,-3,11,14,3,1,-3]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [9,-3,11,14,3,1,-3]}))))
  , test "9" (\_ -> equalHack "[12,-6,-1,14,13,14,8,13,-9,-14,-1,1,-16,-3]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [12,-6,-1,14,13,14,8,13,-9,-14,-1,1,-16,-3]}))))
  , test "10" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "11" (\_ -> equalHack "[-4,-17,19,4,-20,-9,16,-18,7,14,15,-13,19,-3,6,-17]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-4,-17,19,4,-20,-9,16,-18,7,14,15,-13,19,-3,6,-17]}))))
  ]

simplerecordEncode03 : Test
simplerecordEncode03 = describe "SimpleRecord encode 03"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[-1,1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-1,1]}))))
  , test "3" (\_ -> equalHack "{\"qux\":[-1,-3,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-1,-3,4]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[-4,2,2,0,1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-4,2,2,0,1]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[7,-6,-8,-3,1,-7,3,0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [7,-6,-8,-3,1,-7,3,0]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[9,-9,-6,-3,-4,10,3,0,-6,-3]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [9,-9,-6,-3,-4,10,3,0,-6,-3]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[7,5,-5,2,0,-11]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [7,5,-5,2,0,-11]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "9" (\_ -> equalHack "{\"qux\":[-1,1,15,10,8,-8,12,-11,-11,4,3,11,-8,13,10,-14]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-1,1,15,10,8,-8,12,-11,-11,4,3,11,-8,13,10,-14]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-17,-17,-16,-15,3,17,13]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-17,-17,-16,-15,3,17,13]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[17,-19,5,13,-13,-5,9,-13,-18,17,-10,-8,-7,-20,-10,9]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [17,-19,5,13,-13,-5,9,-13,-18,17,-10,-8,-7,-20,-10,9]}))))
  ]

simplerecordEncode04 : Test
simplerecordEncode04 = describe "SimpleRecord encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "2" (\_ -> equalHack "[0,2]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [0,2]}))))
  , test "3" (\_ -> equalHack "[-2,-2,0,3]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-2,-2,0,3]}))))
  , test "4" (\_ -> equalHack "[-2,-4,-2,-4,0]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-2,-4,-2,-4,0]}))))
  , test "5" (\_ -> equalHack "[8,1,6,2,2,3,-1]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [8,1,6,2,2,3,-1]}))))
  , test "6" (\_ -> equalHack "[-4,2,-8,-5,-3,-2,10,-8,0]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-4,2,-8,-5,-3,-2,10,-8,0]}))))
  , test "7" (\_ -> equalHack "[6,5,-9,5,-1]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [6,5,-9,5,-1]}))))
  , test "8" (\_ -> equalHack "[-6,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-6,-7]}))))
  , test "9" (\_ -> equalHack "[13,-6,3,3,3,15,-3,-2,-2,15]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [13,-6,3,3,3,15,-3,-2,-2,15]}))))
  , test "10" (\_ -> equalHack "[17,18,-11,11,7,-4,-15,13,9,15,11]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [17,18,-11,11,7,-4,-15,13,9,15,11]}))))
  , test "11" (\_ -> equalHack "[2,-18,3,2,10,14,20,-8,17,3,20,4,-17,18,10,-7,-12]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [2,-18,3,2,10,14,20,-8,17,3,20,4,-17,18,10,-7,-12]}))))
  ]

simplerecordDecode01 : Test
simplerecordDecode01 = describe "SimpleRecord decode 01"
  [ test "1" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord01 {qux = [2,0,-2,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[2,0,-2,3]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord01 {qux = [2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[2]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord01 {qux = [1,6,5,7,-2,8,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[1,6,5,7,-2,8,4]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord01 {qux = [4,10,0,-2,-10,-2,3,0,-9]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[4,10,0,-2,-10,-2,3,0,-9]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord01 {qux = [-7,-3,5,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-7,-3,5,4]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord01 {qux = [-9,4,-1,-12,2,-8,11,-12,7,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-9,4,-1,-12,2,-8,11,-12,7,-5]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord01 {qux = [-6,-9,3,-8,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-6,-9,3,-8,6]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord01 {qux = [9,-15,-8,15,-7,-1,14,-7,0,14,-8,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[9,-15,-8,15,-7,-1,14,-7,0,14,-8,-7]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord01 {qux = [-11,-6,-12,-19,11,7,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-11,-6,-12,-19,11,7,-1]}"))
  ]

simplerecordDecode02 : Test
simplerecordDecode02 = describe "SimpleRecord decode 02"
  [ test "1" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord02 {qux = [-1,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-1,2]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord02 {qux = [-1,-5,-7,4,-5,0,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-1,-5,-7,4,-5,0,-2]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord02 {qux = [-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-5]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord02 {qux = [8,-6,-11,4,5,9,0,-12,-6,3,11]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[8,-6,-11,4,5,9,0,-12,-6,3,11]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord02 {qux = [9,-3,11,14,3,1,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[9,-3,11,14,3,1,-3]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord02 {qux = [12,-6,-1,14,13,14,8,13,-9,-14,-1,1,-16,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[12,-6,-1,14,13,14,8,13,-9,-14,-1,1,-16,-3]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord02 {qux = [-4,-17,19,4,-20,-9,16,-18,7,14,15,-13,19,-3,6,-17]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-4,-17,19,4,-20,-9,16,-18,7,14,15,-13,19,-3,6,-17]"))
  ]

simplerecordDecode03 : Test
simplerecordDecode03 = describe "SimpleRecord decode 03"
  [ test "1" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord03 {qux = [-1,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,1]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord03 {qux = [-1,-3,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,-3,4]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord03 {qux = [-4,2,2,0,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-4,2,2,0,1]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord03 {qux = [7,-6,-8,-3,1,-7,3,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[7,-6,-8,-3,1,-7,3,0]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord03 {qux = [9,-9,-6,-3,-4,10,3,0,-6,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[9,-9,-6,-3,-4,10,3,0,-6,-3]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord03 {qux = [7,5,-5,2,0,-11]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[7,5,-5,2,0,-11]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord03 {qux = [-1,1,15,10,8,-8,12,-11,-11,4,3,11,-8,13,10,-14]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,1,15,10,8,-8,12,-11,-11,4,3,11,-8,13,10,-14]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord03 {qux = [-17,-17,-16,-15,3,17,13]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-17,-17,-16,-15,3,17,13]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord03 {qux = [17,-19,5,13,-13,-5,9,-13,-18,17,-10,-8,-7,-20,-10,9]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[17,-19,5,13,-13,-5,9,-13,-18,17,-10,-8,-7,-20,-10,9]}"))
  ]

simplerecordDecode04 : Test
simplerecordDecode04 = describe "SimpleRecord decode 04"
  [ test "1" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord04 {qux = [0,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[0,2]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord04 {qux = [-2,-2,0,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-2,-2,0,3]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord04 {qux = [-2,-4,-2,-4,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-2,-4,-2,-4,0]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord04 {qux = [8,1,6,2,2,3,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[8,1,6,2,2,3,-1]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord04 {qux = [-4,2,-8,-5,-3,-2,10,-8,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-4,2,-8,-5,-3,-2,10,-8,0]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord04 {qux = [6,5,-9,5,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[6,5,-9,5,-1]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord04 {qux = [-6,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-6,-7]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord04 {qux = [13,-6,3,3,3,15,-3,-2,-2,15]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[13,-6,3,3,3,15,-3,-2,-2,15]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord04 {qux = [17,18,-11,11,7,-4,-15,13,9,15,11]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[17,18,-11,11,7,-4,-15,13,9,15,11]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord04 {qux = [2,-18,3,2,10,14,20,-8,17,3,20,4,-17,18,10,-7,-12]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[2,-18,3,2,10,14,20,-8,17,3,20,4,-17,18,10,-7,-12]"))
  ]

sumEncodeUntagged : Test
sumEncodeUntagged = describe "Sum encode Untagged"
  [ test "1" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList []))))
  , test "3" (\_ -> equalHack "[-4,-4,4]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [-4,-4,4]))))
  , test "4" (\_ -> equalHack "6"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 6))))
  , test "5" (\_ -> equalHack "[7,8,2,3,-7]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [7,8,2,3,-7]))))
  , test "6" (\_ -> equalHack "[5]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [5]))))
  , test "7" (\_ -> equalHack "7"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 7))))
  , test "8" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "9" (\_ -> equalHack "[3,-8,-2,-5]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [3,-8,-2,-5]))))
  , test "10" (\_ -> equalHack "[7,1]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [7,1]))))
  , test "11" (\_ -> equalHack "-18"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-18)))))
  ]

sumDecodeUntagged : Test
sumDecodeUntagged = describe "Sum decode Untagged"
  [ test "1" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "2" (\_ -> equal (Ok (SMList [])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (SMList [-4,-4,4])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-4,-4,4]"))
  , test "4" (\_ -> equal (Ok (SMInt 6)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "6"))
  , test "5" (\_ -> equal (Ok (SMList [7,8,2,3,-7])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[7,8,2,3,-7]"))
  , test "6" (\_ -> equal (Ok (SMList [5])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[5]"))
  , test "7" (\_ -> equal (Ok (SMInt 7)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "7"))
  , test "8" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "9" (\_ -> equal (Ok (SMList [3,-8,-2,-5])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[3,-8,-2,-5]"))
  , test "10" (\_ -> equal (Ok (SMList [7,1])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[7,1]"))
  , test "11" (\_ -> equal (Ok (SMInt (-18))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-18"))
  ]

sumEncodeIncludeUnit : Test
sumEncodeIncludeUnit = describe "Sum encode IncludeUnit"
  [ test "1" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  , test "3" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,0]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-3,0]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[0],[-5,2,-5,2]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [0] [-5,2,-5,2]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,3,3,-5,2,-4,6]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-7,3,3,-5,2,-4,6]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-4,-8,9,-2,-10]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-4,-8,9,-2,-10]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  , test "8" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  , test "9" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[16,3,-5,-10,-14,-15,16,16]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [16,3,-5,-10,-14,-15,16,16]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[12,18,-9,12,-17,11,14,-6,-16,8,-4,12,7,-10,16,1,-18,-14],[3,7,9,9]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [12,18,-9,12,-17,11,14,-6,-16,8,-4,12,7,-10,16,1,-18,-14] [3,7,9,9]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[4,-2,19]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [4,-2,19]))))
  ]

sumDecodeIncludeUnit : Test
sumDecodeIncludeUnit = describe "Sum decode IncludeUnit"
  [ test "1" (\_ -> equal (Ok (SumIncludeUnitOne [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[]}"))
  , test "2" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  , test "3" (\_ -> equal (Ok (SumIncludeUnitOne [-3,0])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,0]}"))
  , test "4" (\_ -> equal (Ok (SumIncludeUnitTwo [0] [-5,2,-5,2])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[0],[-5,2,-5,2]]}"))
  , test "5" (\_ -> equal (Ok (SumIncludeUnitOne [-7,3,3,-5,2,-4,6])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-7,3,3,-5,2,-4,6]}"))
  , test "6" (\_ -> equal (Ok (SumIncludeUnitOne [-4,-8,9,-2,-10])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-4,-8,9,-2,-10]}"))
  , test "7" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  , test "8" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  , test "9" (\_ -> equal (Ok (SumIncludeUnitOne [16,3,-5,-10,-14,-15,16,16])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[16,3,-5,-10,-14,-15,16,16]}"))
  , test "10" (\_ -> equal (Ok (SumIncludeUnitTwo [12,18,-9,12,-17,11,14,-6,-16,8,-4,12,7,-10,16,1,-18,-14] [3,7,9,9])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[12,18,-9,12,-17,11,14,-6,-16,8,-4,12,7,-10,16,1,-18,-14],[3,7,9,9]]}"))
  , test "11" (\_ -> equal (Ok (SumIncludeUnitOne [4,-2,19])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[4,-2,19]}"))
  ]

ntDecode1 : Test
ntDecode1 = describe "NT decode 1"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "2" (\_ -> equal (Ok ([-2,-1])) (Json.Decode.decodeString jsonDecNT1 "[-2,-1]"))
  , test "3" (\_ -> equal (Ok ([2,-2])) (Json.Decode.decodeString jsonDecNT1 "[2,-2]"))
  , test "4" (\_ -> equal (Ok ([6,2])) (Json.Decode.decodeString jsonDecNT1 "[6,2]"))
  , test "5" (\_ -> equal (Ok ([1,-5,-3,-6])) (Json.Decode.decodeString jsonDecNT1 "[1,-5,-3,-6]"))
  , test "6" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "7" (\_ -> equal (Ok ([0,9,-3,-11,10,11,0,1,-1,-9])) (Json.Decode.decodeString jsonDecNT1 "[0,9,-3,-11,10,11,0,1,-1,-9]"))
  , test "8" (\_ -> equal (Ok ([6,0,0,-11,-6,-14,-8,13,13,-1,0,-2])) (Json.Decode.decodeString jsonDecNT1 "[6,0,0,-11,-6,-14,-8,13,13,-1,0,-2]"))
  , test "9" (\_ -> equal (Ok ([-5,16,3,-10,13,5,-1,-7,5,13,-9])) (Json.Decode.decodeString jsonDecNT1 "[-5,16,3,-10,13,5,-1,-7,5,13,-9]"))
  , test "10" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "11" (\_ -> equal (Ok ([17,5])) (Json.Decode.decodeString jsonDecNT1 "[17,5]"))
  ]

ntEncode1 : Test
ntEncode1 = describe "NT encode 1"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "2" (\_ -> equalHack "[-2,-1]"(Json.Encode.encode 0 (jsonEncNT1 ([-2,-1]))))
  , test "3" (\_ -> equalHack "[2,-2]"(Json.Encode.encode 0 (jsonEncNT1 ([2,-2]))))
  , test "4" (\_ -> equalHack "[6,2]"(Json.Encode.encode 0 (jsonEncNT1 ([6,2]))))
  , test "5" (\_ -> equalHack "[1,-5,-3,-6]"(Json.Encode.encode 0 (jsonEncNT1 ([1,-5,-3,-6]))))
  , test "6" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "7" (\_ -> equalHack "[0,9,-3,-11,10,11,0,1,-1,-9]"(Json.Encode.encode 0 (jsonEncNT1 ([0,9,-3,-11,10,11,0,1,-1,-9]))))
  , test "8" (\_ -> equalHack "[6,0,0,-11,-6,-14,-8,13,13,-1,0,-2]"(Json.Encode.encode 0 (jsonEncNT1 ([6,0,0,-11,-6,-14,-8,13,13,-1,0,-2]))))
  , test "9" (\_ -> equalHack "[-5,16,3,-10,13,5,-1,-7,5,13,-9]"(Json.Encode.encode 0 (jsonEncNT1 ([-5,16,3,-10,13,5,-1,-7,5,13,-9]))))
  , test "10" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "11" (\_ -> equalHack "[17,5]"(Json.Encode.encode 0 (jsonEncNT1 ([17,5]))))
  ]

ntDecode2 : Test
ntDecode2 = describe "NT decode 2"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "2" (\_ -> equal (Ok ([1,2])) (Json.Decode.decodeString jsonDecNT2 "[1,2]"))
  , test "3" (\_ -> equal (Ok ([-2,-1,-1])) (Json.Decode.decodeString jsonDecNT2 "[-2,-1,-1]"))
  , test "4" (\_ -> equal (Ok ([-1,-5,0,-5])) (Json.Decode.decodeString jsonDecNT2 "[-1,-5,0,-5]"))
  , test "5" (\_ -> equal (Ok ([-7,6,5,3])) (Json.Decode.decodeString jsonDecNT2 "[-7,6,5,3]"))
  , test "6" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "7" (\_ -> equal (Ok ([1,4,5,7,2,5,12])) (Json.Decode.decodeString jsonDecNT2 "[1,4,5,7,2,5,12]"))
  , test "8" (\_ -> equal (Ok ([6,5,13,-3])) (Json.Decode.decodeString jsonDecNT2 "[6,5,13,-3]"))
  , test "9" (\_ -> equal (Ok ([5,16,-7,1,-15,-3,3,-8,-13,-15,3])) (Json.Decode.decodeString jsonDecNT2 "[5,16,-7,1,-15,-3,3,-8,-13,-15,3]"))
  , test "10" (\_ -> equal (Ok ([-16,-4,3,-1,6,9,8,4])) (Json.Decode.decodeString jsonDecNT2 "[-16,-4,3,-1,6,9,8,4]"))
  , test "11" (\_ -> equal (Ok ([19,-15,-4,-5,-14])) (Json.Decode.decodeString jsonDecNT2 "[19,-15,-4,-5,-14]"))
  ]

ntEncode2 : Test
ntEncode2 = describe "NT encode 2"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "2" (\_ -> equalHack "[1,2]"(Json.Encode.encode 0 (jsonEncNT2 ([1,2]))))
  , test "3" (\_ -> equalHack "[-2,-1,-1]"(Json.Encode.encode 0 (jsonEncNT2 ([-2,-1,-1]))))
  , test "4" (\_ -> equalHack "[-1,-5,0,-5]"(Json.Encode.encode 0 (jsonEncNT2 ([-1,-5,0,-5]))))
  , test "5" (\_ -> equalHack "[-7,6,5,3]"(Json.Encode.encode 0 (jsonEncNT2 ([-7,6,5,3]))))
  , test "6" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "7" (\_ -> equalHack "[1,4,5,7,2,5,12]"(Json.Encode.encode 0 (jsonEncNT2 ([1,4,5,7,2,5,12]))))
  , test "8" (\_ -> equalHack "[6,5,13,-3]"(Json.Encode.encode 0 (jsonEncNT2 ([6,5,13,-3]))))
  , test "9" (\_ -> equalHack "[5,16,-7,1,-15,-3,3,-8,-13,-15,3]"(Json.Encode.encode 0 (jsonEncNT2 ([5,16,-7,1,-15,-3,3,-8,-13,-15,3]))))
  , test "10" (\_ -> equalHack "[-16,-4,3,-1,6,9,8,4]"(Json.Encode.encode 0 (jsonEncNT2 ([-16,-4,3,-1,6,9,8,4]))))
  , test "11" (\_ -> equalHack "[19,-15,-4,-5,-14]"(Json.Encode.encode 0 (jsonEncNT2 ([19,-15,-4,-5,-14]))))
  ]

ntDecode3 : Test
ntDecode3 = describe "NT decode 3"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "2" (\_ -> equal (Ok ([-1])) (Json.Decode.decodeString jsonDecNT3 "[-1]"))
  , test "3" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "4" (\_ -> equal (Ok ([1,-2,-1,0,6])) (Json.Decode.decodeString jsonDecNT3 "[1,-2,-1,0,6]"))
  , test "5" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "6" (\_ -> equal (Ok ([-1,-5,-8,-1,4,-8])) (Json.Decode.decodeString jsonDecNT3 "[-1,-5,-8,-1,4,-8]"))
  , test "7" (\_ -> equal (Ok ([6,4,4,8,1,-10,-11,-2,4,-10,-2,-7])) (Json.Decode.decodeString jsonDecNT3 "[6,4,4,8,1,-10,-11,-2,4,-10,-2,-7]"))
  , test "8" (\_ -> equal (Ok ([-6,12,-2,-10])) (Json.Decode.decodeString jsonDecNT3 "[-6,12,-2,-10]"))
  , test "9" (\_ -> equal (Ok ([12,7,1,7,-14,2,5,4])) (Json.Decode.decodeString jsonDecNT3 "[12,7,1,7,-14,2,5,4]"))
  , test "10" (\_ -> equal (Ok ([10,12,-4,10,17,14,8,8,2,6,-16,16,-17,7,-18])) (Json.Decode.decodeString jsonDecNT3 "[10,12,-4,10,17,14,8,8,2,6,-16,16,-17,7,-18]"))
  , test "11" (\_ -> equal (Ok ([-3,-8,19,14,3,20,19,-12])) (Json.Decode.decodeString jsonDecNT3 "[-3,-8,19,14,3,20,19,-12]"))
  ]

ntEncode3 : Test
ntEncode3 = describe "NT encode 3"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "2" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncNT3 ([-1]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "4" (\_ -> equalHack "[1,-2,-1,0,6]"(Json.Encode.encode 0 (jsonEncNT3 ([1,-2,-1,0,6]))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "6" (\_ -> equalHack "[-1,-5,-8,-1,4,-8]"(Json.Encode.encode 0 (jsonEncNT3 ([-1,-5,-8,-1,4,-8]))))
  , test "7" (\_ -> equalHack "[6,4,4,8,1,-10,-11,-2,4,-10,-2,-7]"(Json.Encode.encode 0 (jsonEncNT3 ([6,4,4,8,1,-10,-11,-2,4,-10,-2,-7]))))
  , test "8" (\_ -> equalHack "[-6,12,-2,-10]"(Json.Encode.encode 0 (jsonEncNT3 ([-6,12,-2,-10]))))
  , test "9" (\_ -> equalHack "[12,7,1,7,-14,2,5,4]"(Json.Encode.encode 0 (jsonEncNT3 ([12,7,1,7,-14,2,5,4]))))
  , test "10" (\_ -> equalHack "[10,12,-4,10,17,14,8,8,2,6,-16,16,-17,7,-18]"(Json.Encode.encode 0 (jsonEncNT3 ([10,12,-4,10,17,14,8,8,2,6,-16,16,-17,7,-18]))))
  , test "11" (\_ -> equalHack "[-3,-8,19,14,3,20,19,-12]"(Json.Encode.encode 0 (jsonEncNT3 ([-3,-8,19,14,3,20,19,-12]))))
  ]

ntDecode4 : Test
ntDecode4 = describe "NT decode 4"
  [ test "1" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (NT4 {foo = [-2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-2]}"))
  , test "3" (\_ -> equal (Ok (NT4 {foo = [4,-1,1,2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[4,-1,1,2]}"))
  , test "4" (\_ -> equal (Ok (NT4 {foo = [6]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[6]}"))
  , test "5" (\_ -> equal (Ok (NT4 {foo = [3,4,0,-5]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[3,4,0,-5]}"))
  , test "6" (\_ -> equal (Ok (NT4 {foo = [8,0,5,-1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[8,0,5,-1]}"))
  , test "7" (\_ -> equal (Ok (NT4 {foo = [4,6,2,10,2,-2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[4,6,2,10,2,-2]}"))
  , test "8" (\_ -> equal (Ok (NT4 {foo = [-1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-1]}"))
  , test "9" (\_ -> equal (Ok (NT4 {foo = [-2,10,16,-7]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-2,10,16,-7]}"))
  , test "10" (\_ -> equal (Ok (NT4 {foo = [-12,5,-2,0,-3,16]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-12,5,-2,0,-3,16]}"))
  , test "11" (\_ -> equal (Ok (NT4 {foo = [4,9,13,-9,7,-13,-14,-6,-10,-1,-18,-18,1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[4,9,13,-9,7,-13,-14,-6,-10,-1,-18,-18,1]}"))
  ]

ntEncode4 : Test
ntEncode4 = describe "NT encode 4"
  [ test "1" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "2" (\_ -> equalHack "{\"foo\":[-2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-2]}))))
  , test "3" (\_ -> equalHack "{\"foo\":[4,-1,1,2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [4,-1,1,2]}))))
  , test "4" (\_ -> equalHack "{\"foo\":[6]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [6]}))))
  , test "5" (\_ -> equalHack "{\"foo\":[3,4,0,-5]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [3,4,0,-5]}))))
  , test "6" (\_ -> equalHack "{\"foo\":[8,0,5,-1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [8,0,5,-1]}))))
  , test "7" (\_ -> equalHack "{\"foo\":[4,6,2,10,2,-2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [4,6,2,10,2,-2]}))))
  , test "8" (\_ -> equalHack "{\"foo\":[-1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-1]}))))
  , test "9" (\_ -> equalHack "{\"foo\":[-2,10,16,-7]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-2,10,16,-7]}))))
  , test "10" (\_ -> equalHack "{\"foo\":[-12,5,-2,0,-3,16]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-12,5,-2,0,-3,16]}))))
  , test "11" (\_ -> equalHack "{\"foo\":[4,9,13,-9,7,-13,-14,-6,-10,-1,-18,-18,1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [4,9,13,-9,7,-13,-14,-6,-10,-1,-18,-18,1]}))))
  ]

