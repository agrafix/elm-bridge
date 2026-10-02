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
              , recordDecodeNestTuple
              ]

recordEncode : Test
recordEncode = describe "Record encoding checks"
              [ recordEncode1
              , recordEncode2
              , recordEncodeNestTuple
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
   Json.Decode.succeed (\pfoo pbar pbaz pqux pjmap -> Record1 { foo = pfoo, bar = pbar, baz = pbaz, qux = pqux, jmap = pjmap })
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
   Json.Decode.succeed (\pfoo pbar pbaz pqux -> Record2 { foo = pfoo, bar = pbar, baz = pbaz, qux = pqux })
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



type RecordNestTuple a =
    RecordNestTuple (a, (a, a))

jsonDecRecordNestTuple : Json.Decode.Decoder a -> Json.Decode.Decoder ( RecordNestTuple a )
jsonDecRecordNestTuple localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map RecordNestTuple (Json.Decode.map2 tuple2 (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (Json.Decode.map2 tuple2 (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))))


jsonEncRecordNestTuple : (a -> Value) -> RecordNestTuple a -> Value
jsonEncRecordNestTuple localEncoder_a(RecordNestTuple v1) =
    (\(t1,t2) -> Json.Encode.list identity [(localEncoder_a) t1,((\(t3,t4) -> Json.Encode.list identity [(localEncoder_a) t3,(localEncoder_a) t4])) t2]) v1



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
            , ("Sum01D", Json.Decode.lazy (\_ -> Json.Decode.map Sum01D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum01E", Json.Decode.lazy (\_ -> Json.Decode.map Sum01E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum02D", Json.Decode.lazy (\_ -> Json.Decode.map Sum02D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum02E", Json.Decode.lazy (\_ -> Json.Decode.map Sum02E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum03D", Json.Decode.lazy (\_ -> Json.Decode.map Sum03D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum03E", Json.Decode.lazy (\_ -> Json.Decode.map Sum03E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum04D", Json.Decode.lazy (\_ -> Json.Decode.map Sum04D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum04E", Json.Decode.lazy (\_ -> Json.Decode.map Sum04E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum05D", Json.Decode.lazy (\_ -> Json.Decode.map Sum05D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum05E", Json.Decode.lazy (\_ -> Json.Decode.map Sum05E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum06D", Json.Decode.lazy (\_ -> Json.Decode.map Sum06D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum06E", Json.Decode.lazy (\_ -> Json.Decode.map Sum06E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum07D", Json.Decode.lazy (\_ -> Json.Decode.map Sum07D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum07E", Json.Decode.lazy (\_ -> Json.Decode.map Sum07E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum08D", Json.Decode.lazy (\_ -> Json.Decode.map Sum08D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum08E", Json.Decode.lazy (\_ -> Json.Decode.map Sum08E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum09D", Json.Decode.lazy (\_ -> Json.Decode.map Sum09D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum09E", Json.Decode.lazy (\_ -> Json.Decode.map Sum09E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum10D", Json.Decode.lazy (\_ -> Json.Decode.map Sum10D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum10E", Json.Decode.lazy (\_ -> Json.Decode.map Sum10E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum11D", Json.Decode.lazy (\_ -> Json.Decode.map Sum11D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum11E", Json.Decode.lazy (\_ -> Json.Decode.map Sum11E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
            , ("Sum12D", Json.Decode.lazy (\_ -> Json.Decode.map Sum12D (   Json.Decode.succeed (\pfoo -> { foo = pfoo })    |> required "foo" (localDecoder_a))))
            , ("Sum12E", Json.Decode.lazy (\_ -> Json.Decode.map Sum12E (   Json.Decode.succeed (\pbar pbaz -> { bar = pbar, baz = pbaz })    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
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
   Json.Decode.succeed (\pqux -> SimpleRecord01 { qux = pqux })
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
   Json.Decode.succeed (\pqux -> SimpleRecord02 { qux = pqux }) |> custom (localDecoder_a)

jsonEncSimpleRecord02 : (a -> Value) -> SimpleRecord02 a -> Value
jsonEncSimpleRecord02 localEncoder_a (SimpleRecord02 val) =
   localEncoder_a val.qux


type SimpleRecord03 a = SimpleRecord03
   { qux: a
   }

jsonDecSimpleRecord03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord03 a )
jsonDecSimpleRecord03 localDecoder_a =
   Json.Decode.succeed (\pqux -> SimpleRecord03 { qux = pqux })
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
   Json.Decode.succeed (\pqux -> SimpleRecord04 { qux = pqux }) |> custom (localDecoder_a)

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
   Json.Decode.succeed (\pfoo -> NT4 { foo = pfoo })
   |> required "foo" (Json.Decode.list (Json.Decode.int))

jsonEncNT4 : NT4 -> Value
jsonEncNT4  (NT4 val) =
   Json.Encode.object
   [ ("foo", (Json.Encode.list Json.Encode.int) val.foo)
   ]



sumEncode01 : Test
sumEncode01 = describe "Sum encode 01"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[2,2]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [2,2]))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":-4,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = -4, baz = 0}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[],[-1,1,-5,-5,3]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [] [-1,1,-5,-5,3]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[3,4,-7,-8,-7,3]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [3,4,-7,-8,-7,3])))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[9,-10,5,0,0,-6,3,3,-8,10]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = [9,-10,5,0,0,-6,3,3,-8,10]}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-12,-1]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-12,-1]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[14,-14,-4,1,-10,12,5,-7]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [14,-14,-4,1,-10,12,5,-7])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-9,-9,3,1,4,5,-10]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-9,-9,3,1,4,5,-10]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[5,8,4]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [5,8,4])))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":9,\"baz\":-11}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = 9, baz = -11}))))
  ]

sumEncode02 : Test
sumEncode02 = describe "Sum encode 02"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[1]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [1]}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":-4,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = -4, baz = 2}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[6,-1,5,1]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [6,-1,5,1])))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[4,-5,-4,-3,-5,5,5]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [4,-5,-4,-3,-5,5,5])))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[-7,-11,6,1,-12,9,2,-8,1]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [-7,-11,6,1,-12,9,2,-8,1])))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[6,-3,7,-3,11,-9,-12,2,-3,12]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [6,-3,7,-3,11,-9,-12,2,-3,12]}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[-5,2,2,-1,4,-9]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [-5,2,2,-1,4,-9])))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":17,\"baz\":11}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 17, baz = 11}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum02B\",\"content\":[1,-11,-6,-18,12,-9,-15,0,0,-20,15,-8,20,-16,17,-7]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02B (Just [1,-11,-6,-18,12,-9,-15,0,0,-20,15,-8,20,-16,17,-7])))))
  ]

sumEncode03 : Test
sumEncode03 = describe "Sum encode 03"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [])))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-2]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = [-2]}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[0,-4,3,-4]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [0,-4,3,-4]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A []))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":7,\"baz\":-7}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 7, baz = -7}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-9,0,-4,8,7,-10,-3,-5],[9,6,2,2,-9,-2,6,10]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [-9,0,-4,8,7,-10,-3,-5] [9,6,2,2,-9,-2,6,10]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-11,-8,-7,8,-10,-2,-12,-11,-10]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = [-11,-8,-7,8,-10,-2,-12,-11,-10]}))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[8,-6,-10,-12,8,5,13,12,5]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [8,-6,-10,-12,8,5,13,12,5]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[-2,-12,-2,-7,13,-6,-7,8,-11,3,15,6,13,-15,-2]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [-2,-12,-2,-7,13,-6,-7,8,-11,3,15,6,13,-15,-2]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[16,-8,12,8,10,10,14,-4,13,1,17,17,7,11,2]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [16,-8,12,8,10,10,14,-4,13,1,17,17,7,11,2])))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = []}))))
  ]

sumEncode04 : Test
sumEncode04 = describe "Sum encode 04"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[2,-2]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [2,-2]}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[3,4,3,-3]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [] [3,4,3,-3]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[6,6,-5,0]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [6,6,-5,0]}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[-3,2,-1,5,1,-8,7,6]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [-3,2,-1,5,1,-8,7,6]}))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[-8,-1,-10,-9,-1,-8,-6,-6]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [-8,-1,-10,-9,-1,-8,-6,-6])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[-5,8,5,4,2,6,4,0]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [] [-5,8,5,4,2,6,4,0]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[1,14]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [1,14]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":-5,\"baz\":-8}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = -5, baz = -8}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[-2,10,19,5,5,-2,-4,20],[-20,17,6,-18,-18,-17,1,-11,11,19,3,-6]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [-2,10,19,5,5,-2,-4,20] [-20,17,6,-18,-18,-17,1,-11,11,19,3,-6]))))
  ]

sumEncode05 : Test
sumEncode05 = describe "Sum encode 05"
  [ test "1" (\_ -> equalHack "{\"Sum05C\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [] []))))
  , test "2" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-1,\"baz\":-2}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = -1, baz = -2}))))
  , test "3" (\_ -> equalHack "{\"Sum05C\":[[0,0],[-4,0,-1]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [0,0] [-4,0,-1]))))
  , test "4" (\_ -> equalHack "{\"Sum05C\":[[-3,-3,2,3],[-4,-6,4,0]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [-3,-3,2,3] [-4,-6,4,0]))))
  , test "5" (\_ -> equalHack "{\"Sum05B\":[-4,4,5,-2,-2,-7,-4,-4]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [-4,4,5,-2,-2,-7,-4,-4])))))
  , test "6" (\_ -> equalHack "{\"Sum05B\":[1,5,-10,10,-8,-6,-2]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [1,5,-10,10,-8,-6,-2])))))
  , test "7" (\_ -> equalHack "{\"Sum05B\":[-10,6,3,-3,12]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [-10,6,3,-3,12])))))
  , test "8" (\_ -> equalHack "{\"Sum05B\":[10,13,10,-1,-9,-8,-3,-2,-6]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [10,13,10,-1,-9,-8,-3,-2,-6])))))
  , test "9" (\_ -> equalHack "{\"Sum05B\":[-9,16,9,-3,-10,8,6,9,-2,-8,12,8,5]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [-9,16,9,-3,-10,8,6,9,-2,-8,12,8,5])))))
  , test "10" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[-18,4,3,-3,-5,18,12,-1,-14,4,-1,13,-11,0,0,-4]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [-18,4,3,-3,-5,18,12,-1,-14,4,-1,13,-11,0,0,-4]}))))
  , test "11" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[-6,-6,-19,-1,-8,16,15,3,10,4,-5,-19,5,-8]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [-6,-6,-19,-1,-8,16,15,3,10,4,-5,-19,5,-8]}))))
  ]

sumEncode06 : Test
sumEncode06 = describe "Sum encode 06"
  [ test "1" (\_ -> equalHack "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"Sum06B\":[]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [])))))
  , test "3" (\_ -> equalHack "{\"Sum06B\":[-2,-1,3]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [-2,-1,3])))))
  , test "4" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[2]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [2]}))))
  , test "5" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[4,3,-3,-5,-8,2,-2]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [4,3,-3,-5,-8,2,-2]}))))
  , test "6" (\_ -> equalHack "{\"Sum06A\":[2,1,-3,0,-4]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A [2,1,-3,0,-4]))))
  , test "7" (\_ -> equalHack "{\"Sum06C\":[[3,-6,-5,11,-9,3],[1,-4,6,12,-9,-2,11,-5,8,-1,-4,-6]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [3,-6,-5,11,-9,3] [1,-4,6,12,-9,-2,11,-5,8,-1,-4,-6]))))
  , test "8" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[-11,8,5]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [-11,8,5]}))))
  , test "9" (\_ -> equalHack "{\"Sum06E\":{\"bar\":0,\"baz\":-1}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 0, baz = -1}))))
  , test "10" (\_ -> equalHack "{\"Sum06A\":[1,-6,11,10,7,-17,1,-15,16,-10,-16,-16]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A [1,-6,11,10,7,-17,1,-15,16,-10,-16,-16]))))
  , test "11" (\_ -> equalHack "{\"Sum06C\":[[12,-5,13,-11],[]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [12,-5,13,-11] []))))
  ]

sumEncode07 : Test
sumEncode07 = describe "Sum encode 07"
  [ test "1" (\_ -> equalHack "{\"Sum07E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"Sum07B\":[-1,2]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-1,2])))))
  , test "3" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[1,0,2,1]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [1,0,2,1]}))))
  , test "4" (\_ -> equalHack "{\"Sum07B\":[0,5,2,1,-4]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [0,5,2,1,-4])))))
  , test "5" (\_ -> equalHack "{\"Sum07C\":[[-6,-4,5,8,-1,4],[]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [-6,-4,5,8,-1,4] []))))
  , test "6" (\_ -> equalHack "{\"Sum07E\":{\"bar\":-1,\"baz\":-4}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = -1, baz = -4}))))
  , test "7" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[12,5,-6,-4,9,10,6,3,4,10,5,4]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [12,5,-6,-4,9,10,6,3,4,10,5,4]}))))
  , test "8" (\_ -> equalHack "{\"Sum07C\":[[-11,-13,-3,10,-14,5,3,5,-2,2],[-12,4,-10,-4,-14]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [-11,-13,-3,10,-14,5,3,5,-2,2] [-12,4,-10,-4,-14]))))
  , test "9" (\_ -> equalHack "{\"Sum07C\":[[10,-12,13,-5,8,-11,11,12,-16,16,-12,5,1,-4,13],[-9,6,6,1,-8,-13,10,3]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [10,-12,13,-5,8,-11,11,12,-16,16,-12,5,1,-4,13] [-9,6,6,1,-8,-13,10,3]))))
  , test "10" (\_ -> equalHack "{\"Sum07B\":[2,-11,18,-8,-11,9]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [2,-11,18,-8,-11,9])))))
  , test "11" (\_ -> equalHack "{\"Sum07B\":[-13,-15,-20,18,-4,6,-9,16]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-13,-15,-20,18,-4,6,-9,16])))))
  ]

sumEncode08 : Test
sumEncode08 = describe "Sum encode 08"
  [ test "1" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum08E\":{\"bar\":-2,\"baz\":1}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = -2, baz = 1}))))
  , test "3" (\_ -> equalHack "{\"Sum08A\":[]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A []))))
  , test "4" (\_ -> equalHack "{\"Sum08B\":[-6,-4,4,-5,3,6]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [-6,-4,4,-5,3,6])))))
  , test "5" (\_ -> equalHack "{\"Sum08B\":[]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [])))))
  , test "6" (\_ -> equalHack "{\"Sum08E\":{\"bar\":6,\"baz\":4}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 6, baz = 4}))))
  , test "7" (\_ -> equalHack "{\"Sum08E\":{\"bar\":11,\"baz\":10}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 11, baz = 10}))))
  , test "8" (\_ -> equalHack "{\"Sum08E\":{\"bar\":11,\"baz\":-1}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = 11, baz = -1}))))
  , test "9" (\_ -> equalHack "{\"Sum08A\":[-8,5,-5,11,-11]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [-8,5,-5,11,-11]))))
  , test "10" (\_ -> equalHack "{\"Sum08E\":{\"bar\":-3,\"baz\":15}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08E {bar = -3, baz = 15}))))
  , test "11" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[20,-12,8,-15,5,19,-19,0]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [20,-12,8,-15,5,19,-19,0]}))))
  ]

sumEncode09 : Test
sumEncode09 = describe "Sum encode 09"
  [ test "1" (\_ -> equalHack "[\"Sum09A\",[]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A []))))
  , test "2" (\_ -> equalHack "[\"Sum09A\",[2]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [2]))))
  , test "3" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = []}))))
  , test "4" (\_ -> equalHack "[\"Sum09B\",[1,-2,-3,3,-6,6]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [1,-2,-3,3,-6,6])))))
  , test "5" (\_ -> equalHack "[\"Sum09A\",[1,-8,1,3,1,4]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [1,-8,1,3,1,4]))))
  , test "6" (\_ -> equalHack "[\"Sum09B\",[-10]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [-10])))))
  , test "7" (\_ -> equalHack "[\"Sum09B\",[-9,11,8,-3,0]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [-9,11,8,-3,0])))))
  , test "8" (\_ -> equalHack "[\"Sum09E\",{\"bar\":7,\"baz\":-1}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 7, baz = -1}))))
  , test "9" (\_ -> equalHack "[\"Sum09B\",[15,-16,-6,-4,-13]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09B (Just [15,-16,-6,-4,-13])))))
  , test "10" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[4,-7,-6]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [4,-7,-6]}))))
  , test "11" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[5,-18,10,3,16,2,15,-7]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [5,-18,10,3,16,2,15,-7]}))))
  ]

sumEncode10 : Test
sumEncode10 = describe "Sum encode 10"
  [ test "1" (\_ -> equalHack "[\"Sum10E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "[\"Sum10E\",{\"bar\":1,\"baz\":2}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 1, baz = 2}))))
  , test "3" (\_ -> equalHack "[\"Sum10C\",[[4,-1,-1,1],[0]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [4,-1,-1,1] [0]))))
  , test "4" (\_ -> equalHack "[\"Sum10C\",[[6,-4,-6],[-5]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [6,-4,-6] [-5]))))
  , test "5" (\_ -> equalHack "[\"Sum10E\",{\"bar\":-8,\"baz\":5}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = -8, baz = 5}))))
  , test "6" (\_ -> equalHack "[\"Sum10E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 0, baz = 0}))))
  , test "7" (\_ -> equalHack "[\"Sum10C\",[[-5,-9,-8,-11,-3,-6,4,-1,-9],[3]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [-5,-9,-8,-11,-3,-6,4,-1,-9] [3]))))
  , test "8" (\_ -> equalHack "[\"Sum10E\",{\"bar\":8,\"baz\":13}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 8, baz = 13}))))
  , test "9" (\_ -> equalHack "[\"Sum10B\",[14]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [14])))))
  , test "10" (\_ -> equalHack "[\"Sum10A\",[-14,1,-1,15,16,0,5,15]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [-14,1,-1,15,16,0,5,15]))))
  , test "11" (\_ -> equalHack "[\"Sum10C\",[[-11,19,-18,-1],[6,-16,9,-3]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [-11,19,-18,-1] [6,-16,9,-3]))))
  ]

sumEncode11 : Test
sumEncode11 = describe "Sum encode 11"
  [ test "1" (\_ -> equalHack "[\"Sum11E\",{\"bar\":0,\"baz\":0}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "[\"Sum11C\",[[0],[]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [0] []))))
  , test "3" (\_ -> equalHack "[\"Sum11E\",{\"bar\":2,\"baz\":-2}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11E {bar = 2, baz = -2}))))
  , test "4" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[5,6,-6,2,-4,2]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [5,6,-6,2,-4,2]}))))
  , test "5" (\_ -> equalHack "[\"Sum11A\",[6,2,5,-4,8,-5,-3]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [6,2,5,-4,8,-5,-3]))))
  , test "6" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[7]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [7]}))))
  , test "7" (\_ -> equalHack "[\"Sum11A\",[7,-7,-7,12,-3,-11,-4,-4,-9,8,-11,2]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [7,-7,-7,12,-3,-11,-4,-4,-9,8,-11,2]))))
  , test "8" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[-7,-1,-10,4,-7,6,-14,10,-14,13,3,-3,-12]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [-7,-1,-10,4,-7,6,-14,10,-14,13,3,-3,-12]}))))
  , test "9" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[14,2]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [14,2]}))))
  , test "10" (\_ -> equalHack "[\"Sum11C\",[[7,18,17,17],[5,9,3,-8,5,-4,-13,-16,14]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [7,18,17,17] [5,9,3,-8,5,-4,-13,-16,14]))))
  , test "11" (\_ -> equalHack "[\"Sum11C\",[[19,-3,11,3,-4,-11,-16,11,-2,14,-4,-11,-20,-1,-3,10,-1,15,-11,-8],[-7,-13,-20,-8,-10,16,-16,18,5,-15,-1,1,-5,-7,-17,-6,18]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [19,-3,11,3,-4,-11,-16,11,-2,14,-4,-11,-20,-1,-3,10,-1,15,-11,-8] [-7,-13,-20,-8,-10,16,-16,18,5,-15,-1,1,-5,-7,-17,-6,18]))))
  ]

sumEncode12 : Test
sumEncode12 = describe "Sum encode 12"
  [ test "1" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = []}))))
  , test "2" (\_ -> equalHack "[\"Sum12E\",{\"bar\":1,\"baz\":-1}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = 1, baz = -1}))))
  , test "3" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[4,-3]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [4,-3]}))))
  , test "4" (\_ -> equalHack "[\"Sum12E\",{\"bar\":-4,\"baz\":1}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = -4, baz = 1}))))
  , test "5" (\_ -> equalHack "[\"Sum12E\",{\"bar\":-7,\"baz\":3}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = -7, baz = 3}))))
  , test "6" (\_ -> equalHack "[\"Sum12A\",[-3,7,3,-3,-10,-5,5,-8]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [-3,7,3,-3,-10,-5,5,-8]))))
  , test "7" (\_ -> equalHack "[\"Sum12B\",[-12]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [-12])))))
  , test "8" (\_ -> equalHack "[\"Sum12A\",[12]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [12]))))
  , test "9" (\_ -> equalHack "[\"Sum12B\",[-4,14,-10,-15,-2,3]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [-4,14,-10,-15,-2,3])))))
  , test "10" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[18]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [18]}))))
  , test "11" (\_ -> equalHack "[\"Sum12E\",{\"bar\":-5,\"baz\":-7}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = -5, baz = -7}))))
  ]

sumDecode01 : Test
sumDecode01 = describe "Sum decode 01"
  [ test "1" (\_ -> equal (Ok (Sum01E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum01A [2,2])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[2,2]}"))
  , test "3" (\_ -> equal (Ok (Sum01E {bar = -4, baz = 0})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":-4,\"baz\":0}"))
  , test "4" (\_ -> equal (Ok (Sum01C [] [-1,1,-5,-5,3])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[],[-1,1,-5,-5,3]]}"))
  , test "5" (\_ -> equal (Ok (Sum01B (Just [3,4,-7,-8,-7,3]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[3,4,-7,-8,-7,3]}"))
  , test "6" (\_ -> equal (Ok (Sum01D {foo = [9,-10,5,0,0,-6,3,3,-8,10]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[9,-10,5,0,0,-6,3,3,-8,10]}"))
  , test "7" (\_ -> equal (Ok (Sum01A [-12,-1])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-12,-1]}"))
  , test "8" (\_ -> equal (Ok (Sum01B (Just [14,-14,-4,1,-10,12,5,-7]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[14,-14,-4,1,-10,12,5,-7]}"))
  , test "9" (\_ -> equal (Ok (Sum01A [-9,-9,3,1,4,5,-10])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-9,-9,3,1,4,5,-10]}"))
  , test "10" (\_ -> equal (Ok (Sum01B (Just [5,8,4]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[5,8,4]}"))
  , test "11" (\_ -> equal (Ok (Sum01E {bar = 9, baz = -11})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":9,\"baz\":-11}"))
  ]

sumDecode02 : Test
sumDecode02 = describe "Sum decode 02"
  [ test "1" (\_ -> equal (Ok (Sum02E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum02D {foo = [1]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[1]}"))
  , test "3" (\_ -> equal (Ok (Sum02E {bar = -4, baz = 2})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":-4,\"baz\":2}"))
  , test "4" (\_ -> equal (Ok (Sum02B (Just [6,-1,5,1]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[6,-1,5,1]}"))
  , test "5" (\_ -> equal (Ok (Sum02B (Just [4,-5,-4,-3,-5,5,5]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[4,-5,-4,-3,-5,5,5]}"))
  , test "6" (\_ -> equal (Ok (Sum02B (Just []))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[]}"))
  , test "7" (\_ -> equal (Ok (Sum02B (Just [-7,-11,6,1,-12,9,2,-8,1]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[-7,-11,6,1,-12,9,2,-8,1]}"))
  , test "8" (\_ -> equal (Ok (Sum02D {foo = [6,-3,7,-3,11,-9,-12,2,-3,12]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[6,-3,7,-3,11,-9,-12,2,-3,12]}"))
  , test "9" (\_ -> equal (Ok (Sum02B (Just [-5,2,2,-1,4,-9]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[-5,2,2,-1,4,-9]}"))
  , test "10" (\_ -> equal (Ok (Sum02E {bar = 17, baz = 11})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":17,\"baz\":11}"))
  , test "11" (\_ -> equal (Ok (Sum02B (Just [1,-11,-6,-18,12,-9,-15,0,0,-20,15,-8,20,-16,17,-7]))) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02B\",\"content\":[1,-11,-6,-18,12,-9,-15,0,0,-20,15,-8,20,-16,17,-7]}"))
  ]

sumDecode03 : Test
sumDecode03 = describe "Sum decode 03"
  [ test "1" (\_ -> equal (Ok (Sum03B (Just []))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum03D {foo = [-2]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-2]}"))
  , test "3" (\_ -> equal (Ok (Sum03A [0,-4,3,-4])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[0,-4,3,-4]}"))
  , test "4" (\_ -> equal (Ok (Sum03A [])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[]}"))
  , test "5" (\_ -> equal (Ok (Sum03E {bar = 7, baz = -7})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":7,\"baz\":-7}"))
  , test "6" (\_ -> equal (Ok (Sum03C [-9,0,-4,8,7,-10,-3,-5] [9,6,2,2,-9,-2,6,10])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-9,0,-4,8,7,-10,-3,-5],[9,6,2,2,-9,-2,6,10]]}"))
  , test "7" (\_ -> equal (Ok (Sum03D {foo = [-11,-8,-7,8,-10,-2,-12,-11,-10]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-11,-8,-7,8,-10,-2,-12,-11,-10]}"))
  , test "8" (\_ -> equal (Ok (Sum03A [8,-6,-10,-12,8,5,13,12,5])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[8,-6,-10,-12,8,5,13,12,5]}"))
  , test "9" (\_ -> equal (Ok (Sum03A [-2,-12,-2,-7,13,-6,-7,8,-11,3,15,6,13,-15,-2])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[-2,-12,-2,-7,13,-6,-7,8,-11,3,15,6,13,-15,-2]}"))
  , test "10" (\_ -> equal (Ok (Sum03B (Just [16,-8,12,8,10,10,14,-4,13,1,17,17,7,11,2]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[16,-8,12,8,10,10,14,-4,13,1,17,17,7,11,2]}"))
  , test "11" (\_ -> equal (Ok (Sum03D {foo = []})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[]}"))
  ]

sumDecode04 : Test
sumDecode04 = describe "Sum decode 04"
  [ test "1" (\_ -> equal (Ok (Sum04C [] [])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum04D {foo = [2,-2]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[2,-2]}"))
  , test "3" (\_ -> equal (Ok (Sum04C [] [3,4,3,-3])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[3,4,3,-3]]}"))
  , test "4" (\_ -> equal (Ok (Sum04D {foo = [6,6,-5,0]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[6,6,-5,0]}"))
  , test "5" (\_ -> equal (Ok (Sum04D {foo = [-3,2,-1,5,1,-8,7,6]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[-3,2,-1,5,1,-8,7,6]}"))
  , test "6" (\_ -> equal (Ok (Sum04B (Just [-8,-1,-10,-9,-1,-8,-6,-6]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[-8,-1,-10,-9,-1,-8,-6,-6]}"))
  , test "7" (\_ -> equal (Ok (Sum04C [] [-5,8,5,4,2,6,4,0])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[-5,8,5,4,2,6,4,0]]}"))
  , test "8" (\_ -> equal (Ok (Sum04B (Just []))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[]}"))
  , test "9" (\_ -> equal (Ok (Sum04D {foo = [1,14]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[1,14]}"))
  , test "10" (\_ -> equal (Ok (Sum04E {bar = -5, baz = -8})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":-5,\"baz\":-8}"))
  , test "11" (\_ -> equal (Ok (Sum04C [-2,10,19,5,5,-2,-4,20] [-20,17,6,-18,-18,-17,1,-11,11,19,3,-6])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[-2,10,19,5,5,-2,-4,20],[-20,17,6,-18,-18,-17,1,-11,11,19,3,-6]]}"))
  ]

sumDecode05 : Test
sumDecode05 = describe "Sum decode 05"
  [ test "1" (\_ -> equal (Ok (Sum05C [] [])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum05E {bar = -1, baz = -2})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-1,\"baz\":-2}}"))
  , test "3" (\_ -> equal (Ok (Sum05C [0,0] [-4,0,-1])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[0,0],[-4,0,-1]]}"))
  , test "4" (\_ -> equal (Ok (Sum05C [-3,-3,2,3] [-4,-6,4,0])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[-3,-3,2,3],[-4,-6,4,0]]}"))
  , test "5" (\_ -> equal (Ok (Sum05B (Just [-4,4,5,-2,-2,-7,-4,-4]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[-4,4,5,-2,-2,-7,-4,-4]}"))
  , test "6" (\_ -> equal (Ok (Sum05B (Just [1,5,-10,10,-8,-6,-2]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[1,5,-10,10,-8,-6,-2]}"))
  , test "7" (\_ -> equal (Ok (Sum05B (Just [-10,6,3,-3,12]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[-10,6,3,-3,12]}"))
  , test "8" (\_ -> equal (Ok (Sum05B (Just [10,13,10,-1,-9,-8,-3,-2,-6]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[10,13,10,-1,-9,-8,-3,-2,-6]}"))
  , test "9" (\_ -> equal (Ok (Sum05B (Just [-9,16,9,-3,-10,8,6,9,-2,-8,12,8,5]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[-9,16,9,-3,-10,8,6,9,-2,-8,12,8,5]}"))
  , test "10" (\_ -> equal (Ok (Sum05D {foo = [-18,4,3,-3,-5,18,12,-1,-14,4,-1,13,-11,0,0,-4]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[-18,4,3,-3,-5,18,12,-1,-14,4,-1,13,-11,0,0,-4]}}"))
  , test "11" (\_ -> equal (Ok (Sum05D {foo = [-6,-6,-19,-1,-8,16,15,3,10,4,-5,-19,5,-8]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[-6,-6,-19,-1,-8,16,15,3,10,4,-5,-19,5,-8]}}"))
  ]

sumDecode06 : Test
sumDecode06 = describe "Sum decode 06"
  [ test "1" (\_ -> equal (Ok (Sum06E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"))
  , test "2" (\_ -> equal (Ok (Sum06B (Just []))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[]}"))
  , test "3" (\_ -> equal (Ok (Sum06B (Just [-2,-1,3]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[-2,-1,3]}"))
  , test "4" (\_ -> equal (Ok (Sum06D {foo = [2]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[2]}}"))
  , test "5" (\_ -> equal (Ok (Sum06D {foo = [4,3,-3,-5,-8,2,-2]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[4,3,-3,-5,-8,2,-2]}}"))
  , test "6" (\_ -> equal (Ok (Sum06A [2,1,-3,0,-4])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[2,1,-3,0,-4]}"))
  , test "7" (\_ -> equal (Ok (Sum06C [3,-6,-5,11,-9,3] [1,-4,6,12,-9,-2,11,-5,8,-1,-4,-6])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[3,-6,-5,11,-9,3],[1,-4,6,12,-9,-2,11,-5,8,-1,-4,-6]]}"))
  , test "8" (\_ -> equal (Ok (Sum06D {foo = [-11,8,5]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[-11,8,5]}}"))
  , test "9" (\_ -> equal (Ok (Sum06E {bar = 0, baz = -1})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":0,\"baz\":-1}}"))
  , test "10" (\_ -> equal (Ok (Sum06A [1,-6,11,10,7,-17,1,-15,16,-10,-16,-16])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[1,-6,11,10,7,-17,1,-15,16,-10,-16,-16]}"))
  , test "11" (\_ -> equal (Ok (Sum06C [12,-5,13,-11] [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[12,-5,13,-11],[]]}"))
  ]

sumDecode07 : Test
sumDecode07 = describe "Sum decode 07"
  [ test "1" (\_ -> equal (Ok (Sum07E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":0,\"baz\":0}}"))
  , test "2" (\_ -> equal (Ok (Sum07B (Just [-1,2]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-1,2]}"))
  , test "3" (\_ -> equal (Ok (Sum07D {foo = [1,0,2,1]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[1,0,2,1]}}"))
  , test "4" (\_ -> equal (Ok (Sum07B (Just [0,5,2,1,-4]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[0,5,2,1,-4]}"))
  , test "5" (\_ -> equal (Ok (Sum07C [-6,-4,5,8,-1,4] [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-6,-4,5,8,-1,4],[]]}"))
  , test "6" (\_ -> equal (Ok (Sum07E {bar = -1, baz = -4})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":-1,\"baz\":-4}}"))
  , test "7" (\_ -> equal (Ok (Sum07D {foo = [12,5,-6,-4,9,10,6,3,4,10,5,4]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[12,5,-6,-4,9,10,6,3,4,10,5,4]}}"))
  , test "8" (\_ -> equal (Ok (Sum07C [-11,-13,-3,10,-14,5,3,5,-2,2] [-12,4,-10,-4,-14])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-11,-13,-3,10,-14,5,3,5,-2,2],[-12,4,-10,-4,-14]]}"))
  , test "9" (\_ -> equal (Ok (Sum07C [10,-12,13,-5,8,-11,11,12,-16,16,-12,5,1,-4,13] [-9,6,6,1,-8,-13,10,3])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[10,-12,13,-5,8,-11,11,12,-16,16,-12,5,1,-4,13],[-9,6,6,1,-8,-13,10,3]]}"))
  , test "10" (\_ -> equal (Ok (Sum07B (Just [2,-11,18,-8,-11,9]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[2,-11,18,-8,-11,9]}"))
  , test "11" (\_ -> equal (Ok (Sum07B (Just [-13,-15,-20,18,-4,6,-9,16]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-13,-15,-20,18,-4,6,-9,16]}"))
  ]

sumDecode08 : Test
sumDecode08 = describe "Sum decode 08"
  [ test "1" (\_ -> equal (Ok (Sum08D {foo = []})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum08E {bar = -2, baz = 1})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":-2,\"baz\":1}}"))
  , test "3" (\_ -> equal (Ok (Sum08A [])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[]}"))
  , test "4" (\_ -> equal (Ok (Sum08B (Just [-6,-4,4,-5,3,6]))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[-6,-4,4,-5,3,6]}"))
  , test "5" (\_ -> equal (Ok (Sum08B (Just []))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[]}"))
  , test "6" (\_ -> equal (Ok (Sum08E {bar = 6, baz = 4})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":6,\"baz\":4}}"))
  , test "7" (\_ -> equal (Ok (Sum08E {bar = 11, baz = 10})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":11,\"baz\":10}}"))
  , test "8" (\_ -> equal (Ok (Sum08E {bar = 11, baz = -1})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":11,\"baz\":-1}}"))
  , test "9" (\_ -> equal (Ok (Sum08A [-8,5,-5,11,-11])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[-8,5,-5,11,-11]}"))
  , test "10" (\_ -> equal (Ok (Sum08E {bar = -3, baz = 15})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08E\":{\"bar\":-3,\"baz\":15}}"))
  , test "11" (\_ -> equal (Ok (Sum08D {foo = [20,-12,8,-15,5,19,-19,0]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[20,-12,8,-15,5,19,-19,0]}}"))
  ]

sumDecode09 : Test
sumDecode09 = describe "Sum decode 09"
  [ test "1" (\_ -> equal (Ok (Sum09A [])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum09A [2])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[2]]"))
  , test "3" (\_ -> equal (Ok (Sum09D {foo = []})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[]}]"))
  , test "4" (\_ -> equal (Ok (Sum09B (Just [1,-2,-3,3,-6,6]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[1,-2,-3,3,-6,6]]"))
  , test "5" (\_ -> equal (Ok (Sum09A [1,-8,1,3,1,4])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[1,-8,1,3,1,4]]"))
  , test "6" (\_ -> equal (Ok (Sum09B (Just [-10]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[-10]]"))
  , test "7" (\_ -> equal (Ok (Sum09B (Just [-9,11,8,-3,0]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[-9,11,8,-3,0]]"))
  , test "8" (\_ -> equal (Ok (Sum09E {bar = 7, baz = -1})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":7,\"baz\":-1}]"))
  , test "9" (\_ -> equal (Ok (Sum09B (Just [15,-16,-6,-4,-13]))) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09B\",[15,-16,-6,-4,-13]]"))
  , test "10" (\_ -> equal (Ok (Sum09D {foo = [4,-7,-6]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[4,-7,-6]}]"))
  , test "11" (\_ -> equal (Ok (Sum09D {foo = [5,-18,10,3,16,2,15,-7]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[5,-18,10,3,16,2,15,-7]}]"))
  ]

sumDecode10 : Test
sumDecode10 = describe "Sum decode 10"
  [ test "1" (\_ -> equal (Ok (Sum10E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":0,\"baz\":0}]"))
  , test "2" (\_ -> equal (Ok (Sum10E {bar = 1, baz = 2})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":1,\"baz\":2}]"))
  , test "3" (\_ -> equal (Ok (Sum10C [4,-1,-1,1] [0])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[4,-1,-1,1],[0]]]"))
  , test "4" (\_ -> equal (Ok (Sum10C [6,-4,-6] [-5])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[6,-4,-6],[-5]]]"))
  , test "5" (\_ -> equal (Ok (Sum10E {bar = -8, baz = 5})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":-8,\"baz\":5}]"))
  , test "6" (\_ -> equal (Ok (Sum10E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":0,\"baz\":0}]"))
  , test "7" (\_ -> equal (Ok (Sum10C [-5,-9,-8,-11,-3,-6,4,-1,-9] [3])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[-5,-9,-8,-11,-3,-6,4,-1,-9],[3]]]"))
  , test "8" (\_ -> equal (Ok (Sum10E {bar = 8, baz = 13})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":8,\"baz\":13}]"))
  , test "9" (\_ -> equal (Ok (Sum10B (Just [14]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[14]]"))
  , test "10" (\_ -> equal (Ok (Sum10A [-14,1,-1,15,16,0,5,15])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[-14,1,-1,15,16,0,5,15]]"))
  , test "11" (\_ -> equal (Ok (Sum10C [-11,19,-18,-1] [6,-16,9,-3])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[-11,19,-18,-1],[6,-16,9,-3]]]"))
  ]

sumDecode11 : Test
sumDecode11 = describe "Sum decode 11"
  [ test "1" (\_ -> equal (Ok (Sum11E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":0,\"baz\":0}]"))
  , test "2" (\_ -> equal (Ok (Sum11C [0] [])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[0],[]]]"))
  , test "3" (\_ -> equal (Ok (Sum11E {bar = 2, baz = -2})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11E\",{\"bar\":2,\"baz\":-2}]"))
  , test "4" (\_ -> equal (Ok (Sum11D {foo = [5,6,-6,2,-4,2]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[5,6,-6,2,-4,2]}]"))
  , test "5" (\_ -> equal (Ok (Sum11A [6,2,5,-4,8,-5,-3])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[6,2,5,-4,8,-5,-3]]"))
  , test "6" (\_ -> equal (Ok (Sum11D {foo = [7]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[7]}]"))
  , test "7" (\_ -> equal (Ok (Sum11A [7,-7,-7,12,-3,-11,-4,-4,-9,8,-11,2])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[7,-7,-7,12,-3,-11,-4,-4,-9,8,-11,2]]"))
  , test "8" (\_ -> equal (Ok (Sum11D {foo = [-7,-1,-10,4,-7,6,-14,10,-14,13,3,-3,-12]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[-7,-1,-10,4,-7,6,-14,10,-14,13,3,-3,-12]}]"))
  , test "9" (\_ -> equal (Ok (Sum11D {foo = [14,2]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[14,2]}]"))
  , test "10" (\_ -> equal (Ok (Sum11C [7,18,17,17] [5,9,3,-8,5,-4,-13,-16,14])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[7,18,17,17],[5,9,3,-8,5,-4,-13,-16,14]]]"))
  , test "11" (\_ -> equal (Ok (Sum11C [19,-3,11,3,-4,-11,-16,11,-2,14,-4,-11,-20,-1,-3,10,-1,15,-11,-8] [-7,-13,-20,-8,-10,16,-16,18,5,-15,-1,1,-5,-7,-17,-6,18])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[19,-3,11,3,-4,-11,-16,11,-2,14,-4,-11,-20,-1,-3,10,-1,15,-11,-8],[-7,-13,-20,-8,-10,16,-16,18,5,-15,-1,1,-5,-7,-17,-6,18]]]"))
  ]

sumDecode12 : Test
sumDecode12 = describe "Sum decode 12"
  [ test "1" (\_ -> equal (Ok (Sum12D {foo = []})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[]}]"))
  , test "2" (\_ -> equal (Ok (Sum12E {bar = 1, baz = -1})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":1,\"baz\":-1}]"))
  , test "3" (\_ -> equal (Ok (Sum12D {foo = [4,-3]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[4,-3]}]"))
  , test "4" (\_ -> equal (Ok (Sum12E {bar = -4, baz = 1})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":-4,\"baz\":1}]"))
  , test "5" (\_ -> equal (Ok (Sum12E {bar = -7, baz = 3})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":-7,\"baz\":3}]"))
  , test "6" (\_ -> equal (Ok (Sum12A [-3,7,3,-3,-10,-5,5,-8])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-3,7,3,-3,-10,-5,5,-8]]"))
  , test "7" (\_ -> equal (Ok (Sum12B (Just [-12]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[-12]]"))
  , test "8" (\_ -> equal (Ok (Sum12A [12])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[12]]"))
  , test "9" (\_ -> equal (Ok (Sum12B (Just [-4,14,-10,-15,-2,3]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[-4,14,-10,-15,-2,3]]"))
  , test "10" (\_ -> equal (Ok (Sum12D {foo = [18]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[18]}]"))
  , test "11" (\_ -> equal (Ok (Sum12E {bar = -5, baz = -7})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":-5,\"baz\":-7}]"))
  ]

recordDecode1 : Test
recordDecode1 = describe "Record decode 1"
  [ test "1" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"))
  , test "2" (\_ -> equal (Ok (Record1 {foo = 1, bar = Just 1, baz = [], qux = Just [2], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":1,\"bar\":1,\"baz\":[],\"qux\":[2],\"jmap\":{\"a\":-1}}"))
  , test "3" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just (-4), baz = [-3], qux = Just [3,1,0,-3], jmap = fromList [("a",0)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":-4,\"baz\":[-3],\"qux\":[3,1,0,-3],\"jmap\":{\"a\":0}}"))
  , test "4" (\_ -> equal (Ok (Record1 {foo = -6, bar = Just 1, baz = [3,2,1], qux = Just [-2,5], jmap = fromList [("a",-5)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-6,\"bar\":1,\"baz\":[3,2,1],\"qux\":[-2,5],\"jmap\":{\"a\":-5}}"))
  , test "5" (\_ -> equal (Ok (Record1 {foo = -1, bar = Just (-1), baz = [-7,-3], qux = Just [1,-1,-1,-2,-8,-6,4,6], jmap = fromList [("a",4)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":-1,\"baz\":[-7,-3],\"qux\":[1,-1,-1,-2,-8,-6,4,6],\"jmap\":{\"a\":4}}"))
  , test "6" (\_ -> equal (Ok (Record1 {foo = -1, bar = Just 1, baz = [-10,8,10,7,-10,4,7], qux = Just [-10], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":1,\"baz\":[-10,8,10,7,-10,4,7],\"qux\":[-10],\"jmap\":{\"a\":-1}}"))
  , test "7" (\_ -> equal (Ok (Record1 {foo = -10, bar = Just 1, baz = [2,-12], qux = Just [1,11,-2,-3], jmap = fromList [("a",5)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-10,\"bar\":1,\"baz\":[2,-12],\"qux\":[1,11,-2,-3],\"jmap\":{\"a\":5}}"))
  , test "8" (\_ -> equal (Ok (Record1 {foo = 13, bar = Just (-12), baz = [-1,8,-7,-5,-4,-11,-7,-2,-6], qux = Just [-11], jmap = fromList [("a",4)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":13,\"bar\":-12,\"baz\":[-1,8,-7,-5,-4,-11,-7,-2,-6],\"qux\":[-11],\"jmap\":{\"a\":4}}"))
  , test "9" (\_ -> equal (Ok (Record1 {foo = 2, bar = Just 10, baz = [-16,-14,1,12,-3,4,2,-4,-4,-11,1,-3,-10,-16], qux = Just [13,-11,-1,-13,10,4,10,6,4,-11,-5,-4,-3,-15,1,-13], jmap = fromList [("a",-4)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":2,\"bar\":10,\"baz\":[-16,-14,1,12,-3,4,2,-4,-4,-11,1,-3,-10,-16],\"qux\":[13,-11,-1,-13,10,4,10,6,4,-11,-5,-4,-3,-15,1,-13],\"jmap\":{\"a\":-4}}"))
  , test "10" (\_ -> equal (Ok (Record1 {foo = 13, bar = Just 2, baz = [-5,-6,15,-18,8,10,-14,-18,-2,-4,6,14], qux = Just [11,-15,4,16,0,0,-7,-11,12,-12,9,-9], jmap = fromList [("a",-15)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":13,\"bar\":2,\"baz\":[-5,-6,15,-18,8,10,-14,-18,-2,-4,6,14],\"qux\":[11,-15,4,16,0,0,-7,-11,12,-12,9,-9],\"jmap\":{\"a\":-15}}"))
  , test "11" (\_ -> equal (Ok (Record1 {foo = 14, bar = Just 15, baz = [6,19,-4,-7], qux = Just [15,15], jmap = fromList [("a",-20)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":14,\"bar\":15,\"baz\":[6,19,-4,-7],\"qux\":[15,15],\"jmap\":{\"a\":-20}}"))
  ]

recordDecode2 : Test
recordDecode2 = describe "Record decode 2"
  [ test "1" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (Record2 {foo = -1, bar = Just (-1), baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-1,\"bar\":-1,\"baz\":[],\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just (-4), baz = [1], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":1,\"bar\":-4,\"baz\":[1],\"qux\":[]}"))
  , test "4" (\_ -> equal (Ok (Record2 {foo = -5, bar = Just 1, baz = [-4], qux = Just [-4,-3,-5]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-5,\"bar\":1,\"baz\":[-4],\"qux\":[-4,-3,-5]}"))
  , test "5" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just (-6), baz = [5,3,-6,-8], qux = Just [8,6,-5,7]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":-6,\"baz\":[5,3,-6,-8],\"qux\":[8,6,-5,7]}"))
  , test "6" (\_ -> equal (Ok (Record2 {foo = -5, bar = Just (-4), baz = [-1,8,1,9,8,-6,10,-6,-5], qux = Just [-8,-3,1,-3,-7,5,4,2]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-5,\"bar\":-4,\"baz\":[-1,8,1,9,8,-6,10,-6,-5],\"qux\":[-8,-3,1,-3,-7,5,4,2]}"))
  , test "7" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just (-9), baz = [4,-2,-4,-6,4,3,12,-2], qux = Just [1,-2,1,9,1,-4,-1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":1,\"bar\":-9,\"baz\":[4,-2,-4,-6,4,3,12,-2],\"qux\":[1,-2,1,9,1,-4,-1]}"))
  , test "8" (\_ -> equal (Ok (Record2 {foo = -14, bar = Just (-11), baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-14,\"bar\":-11,\"baz\":[],\"qux\":[]}"))
  , test "9" (\_ -> equal (Ok (Record2 {foo = 14, bar = Just 6, baz = [2,3,-5], qux = Just [15,3,-9,-2,5,8,-8,7,5,5,-15,-7,-14,14,-5,11]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":14,\"bar\":6,\"baz\":[2,3,-5],\"qux\":[15,3,-9,-2,5,8,-8,7,5,5,-15,-7,-14,14,-5,11]}"))
  , test "10" (\_ -> equal (Ok (Record2 {foo = 7, bar = Just (-9), baz = [-1,-17,-18,-6,-18,-10,-7,3,-14,-15,-6,-5,18,-4,-1], qux = Just [1,-17,13,3,7,18,-18,-18,-14,-14,1,-3,12,-16,-13,-10,4]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":7,\"bar\":-9,\"baz\":[-1,-17,-18,-6,-18,-10,-7,3,-14,-15,-6,-5,18,-4,-1],\"qux\":[1,-17,13,3,7,18,-18,-18,-14,-14,1,-3,12,-16,-13,-10,4]}"))
  , test "11" (\_ -> equal (Ok (Record2 {foo = -20, bar = Just (-13), baz = [], qux = Just [19,12,-8,-7,-13,-12]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"foo\":-20,\"bar\":-13,\"baz\":[],\"qux\":[19,12,-8,-7,-13,-12]}"))
  ]

recordEncode1 : Test
recordEncode1 = describe "Record encode 1"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]}))))
  , test "2" (\_ -> equalHack "{\"foo\":1,\"bar\":1,\"baz\":[],\"qux\":[2],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 1, bar = Just 1, baz = [], qux = Just [2], jmap = fromList [("a",-1)]}))))
  , test "3" (\_ -> equalHack "{\"foo\":0,\"bar\":-4,\"baz\":[-3],\"qux\":[3,1,0,-3],\"jmap\":{\"a\":0}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just (-4), baz = [-3], qux = Just [3,1,0,-3], jmap = fromList [("a",0)]}))))
  , test "4" (\_ -> equalHack "{\"foo\":-6,\"bar\":1,\"baz\":[3,2,1],\"qux\":[-2,5],\"jmap\":{\"a\":-5}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -6, bar = Just 1, baz = [3,2,1], qux = Just [-2,5], jmap = fromList [("a",-5)]}))))
  , test "5" (\_ -> equalHack "{\"foo\":-1,\"bar\":-1,\"baz\":[-7,-3],\"qux\":[1,-1,-1,-2,-8,-6,4,6],\"jmap\":{\"a\":4}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -1, bar = Just (-1), baz = [-7,-3], qux = Just [1,-1,-1,-2,-8,-6,4,6], jmap = fromList [("a",4)]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-1,\"bar\":1,\"baz\":[-10,8,10,7,-10,4,7],\"qux\":[-10],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -1, bar = Just 1, baz = [-10,8,10,7,-10,4,7], qux = Just [-10], jmap = fromList [("a",-1)]}))))
  , test "7" (\_ -> equalHack "{\"foo\":-10,\"bar\":1,\"baz\":[2,-12],\"qux\":[1,11,-2,-3],\"jmap\":{\"a\":5}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -10, bar = Just 1, baz = [2,-12], qux = Just [1,11,-2,-3], jmap = fromList [("a",5)]}))))
  , test "8" (\_ -> equalHack "{\"foo\":13,\"bar\":-12,\"baz\":[-1,8,-7,-5,-4,-11,-7,-2,-6],\"qux\":[-11],\"jmap\":{\"a\":4}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 13, bar = Just (-12), baz = [-1,8,-7,-5,-4,-11,-7,-2,-6], qux = Just [-11], jmap = fromList [("a",4)]}))))
  , test "9" (\_ -> equalHack "{\"foo\":2,\"bar\":10,\"baz\":[-16,-14,1,12,-3,4,2,-4,-4,-11,1,-3,-10,-16],\"qux\":[13,-11,-1,-13,10,4,10,6,4,-11,-5,-4,-3,-15,1,-13],\"jmap\":{\"a\":-4}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 2, bar = Just 10, baz = [-16,-14,1,12,-3,4,2,-4,-4,-11,1,-3,-10,-16], qux = Just [13,-11,-1,-13,10,4,10,6,4,-11,-5,-4,-3,-15,1,-13], jmap = fromList [("a",-4)]}))))
  , test "10" (\_ -> equalHack "{\"foo\":13,\"bar\":2,\"baz\":[-5,-6,15,-18,8,10,-14,-18,-2,-4,6,14],\"qux\":[11,-15,4,16,0,0,-7,-11,12,-12,9,-9],\"jmap\":{\"a\":-15}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 13, bar = Just 2, baz = [-5,-6,15,-18,8,10,-14,-18,-2,-4,6,14], qux = Just [11,-15,4,16,0,0,-7,-11,12,-12,9,-9], jmap = fromList [("a",-15)]}))))
  , test "11" (\_ -> equalHack "{\"foo\":14,\"bar\":15,\"baz\":[6,19,-4,-7],\"qux\":[15,15],\"jmap\":{\"a\":-20}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 14, bar = Just 15, baz = [6,19,-4,-7], qux = Just [15,15], jmap = fromList [("a",-20)]}))))
  ]

recordEncode2 : Test
recordEncode2 = describe "Record encode 2"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"foo\":-1,\"bar\":-1,\"baz\":[],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -1, bar = Just (-1), baz = [], qux = Just []}))))
  , test "3" (\_ -> equalHack "{\"foo\":1,\"bar\":-4,\"baz\":[1],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just (-4), baz = [1], qux = Just []}))))
  , test "4" (\_ -> equalHack "{\"foo\":-5,\"bar\":1,\"baz\":[-4],\"qux\":[-4,-3,-5]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -5, bar = Just 1, baz = [-4], qux = Just [-4,-3,-5]}))))
  , test "5" (\_ -> equalHack "{\"foo\":0,\"bar\":-6,\"baz\":[5,3,-6,-8],\"qux\":[8,6,-5,7]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just (-6), baz = [5,3,-6,-8], qux = Just [8,6,-5,7]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-5,\"bar\":-4,\"baz\":[-1,8,1,9,8,-6,10,-6,-5],\"qux\":[-8,-3,1,-3,-7,5,4,2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -5, bar = Just (-4), baz = [-1,8,1,9,8,-6,10,-6,-5], qux = Just [-8,-3,1,-3,-7,5,4,2]}))))
  , test "7" (\_ -> equalHack "{\"foo\":1,\"bar\":-9,\"baz\":[4,-2,-4,-6,4,3,12,-2],\"qux\":[1,-2,1,9,1,-4,-1]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just (-9), baz = [4,-2,-4,-6,4,3,12,-2], qux = Just [1,-2,1,9,1,-4,-1]}))))
  , test "8" (\_ -> equalHack "{\"foo\":-14,\"bar\":-11,\"baz\":[],\"qux\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -14, bar = Just (-11), baz = [], qux = Just []}))))
  , test "9" (\_ -> equalHack "{\"foo\":14,\"bar\":6,\"baz\":[2,3,-5],\"qux\":[15,3,-9,-2,5,8,-8,7,5,5,-15,-7,-14,14,-5,11]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 14, bar = Just 6, baz = [2,3,-5], qux = Just [15,3,-9,-2,5,8,-8,7,5,5,-15,-7,-14,14,-5,11]}))))
  , test "10" (\_ -> equalHack "{\"foo\":7,\"bar\":-9,\"baz\":[-1,-17,-18,-6,-18,-10,-7,3,-14,-15,-6,-5,18,-4,-1],\"qux\":[1,-17,13,3,7,18,-18,-18,-14,-14,1,-3,12,-16,-13,-10,4]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 7, bar = Just (-9), baz = [-1,-17,-18,-6,-18,-10,-7,3,-14,-15,-6,-5,18,-4,-1], qux = Just [1,-17,13,3,7,18,-18,-18,-14,-14,1,-3,12,-16,-13,-10,4]}))))
  , test "11" (\_ -> equalHack "{\"foo\":-20,\"bar\":-13,\"baz\":[],\"qux\":[19,12,-8,-7,-13,-12]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -20, bar = Just (-13), baz = [], qux = Just [19,12,-8,-7,-13,-12]}))))
  ]

recordDecodeNestTuple : Test
recordDecodeNestTuple = describe "Record decode NestTuple"
  [ test "1" (\_ -> equal (Ok (RecordNestTuple ([],([],[])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[],[[],[]]]"))
  , test "2" (\_ -> equal (Ok (RecordNestTuple ([-2,2],([2,1],[2,0])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-2,2],[[2,1],[2,0]]]"))
  , test "3" (\_ -> equal (Ok (RecordNestTuple ([],([-1,2,1,1],[2,-1,3,-1])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[],[[-1,2,1,1],[2,-1,3,-1]]]"))
  , test "4" (\_ -> equal (Ok (RecordNestTuple ([6],([0,2],[])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[6],[[0,2],[]]]"))
  , test "5" (\_ -> equal (Ok (RecordNestTuple ([1,4,-4,4,4,-5],([7,6,4,-2,7,2,-3,6],[-5,-7,-1,-7,3,4,3,-8])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[1,4,-4,4,4,-5],[[7,6,4,-2,7,2,-3,6],[-5,-7,-1,-7,3,4,3,-8]]]"))
  , test "6" (\_ -> equal (Ok (RecordNestTuple ([3,-1,-8,-6,-8],([-6,5,8],[-4])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[3,-1,-8,-6,-8],[[-6,5,8],[-4]]]"))
  , test "7" (\_ -> equal (Ok (RecordNestTuple ([-4,-1,10,0,-3,-4,-3,8,-1,-3,-9,9],([-10,-7,7,-4,-1,-6,11,9,10,-11],[1,-7,-6,0,5,-1])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-4,-1,10,0,-3,-4,-3,8,-1,-3,-9,9],[[-10,-7,7,-4,-1,-6,11,9,10,-11],[1,-7,-6,0,5,-1]]]"))
  , test "8" (\_ -> equal (Ok (RecordNestTuple ([-9,11,13,0,5,-12,-8],([2,11,4,-9,12,-3,-6,-12,-7,3,-6,0],[9,1,0])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-9,11,13,0,5,-12,-8],[[2,11,4,-9,12,-3,-6,-12,-7,3,-6,0],[9,1,0]]]"))
  , test "9" (\_ -> equal (Ok (RecordNestTuple ([-7,11,12,12,10,-1,10,5,3,11,-3,9,9,-10],([-9,4,-7,-7,4,10,12,-16,-16,15,-5,-2,1,-2,-1],[12,15,10,-7,9,-10,-8,5,-16,5])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-7,11,12,12,10,-1,10,5,3,11,-3,9,9,-10],[[-9,4,-7,-7,4,10,12,-16,-16,15,-5,-2,1,-2,-1],[12,15,10,-7,9,-10,-8,5,-16,5]]]"))
  , test "10" (\_ -> equal (Ok (RecordNestTuple ([-5,2,11,-3,2,16,9,16,13,13,-14,13,-9,-10,-6,16,-4],([14,10,9,-9,16,2,-9,-14,11,4,-2,2,-7,-17,-13,7],[10,10,-6,9,-7,2,4,9,-7,10,-18,-12])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-5,2,11,-3,2,16,9,16,13,13,-14,13,-9,-10,-6,16,-4],[[14,10,9,-9,16,2,-9,-14,11,4,-2,2,-7,-17,-13,7],[10,10,-6,9,-7,2,4,9,-7,10,-18,-12]]]"))
  , test "11" (\_ -> equal (Ok (RecordNestTuple ([-6,9,11,6,-13,12,18],([5,-13,11,4,-12,-3,10,-17,14,18],[-13,5,-12,-20,-9,2,12,0])))) (Json.Decode.decodeString (jsonDecRecordNestTuple (Json.Decode.list Json.Decode.int)) "[[-6,9,11,6,-13,12,18],[[5,-13,11,4,-12,-3,10,-17,14,18],[-13,5,-12,-20,-9,2,12,0]]]"))
  ]

recordEncodeNestTuple : Test
recordEncodeNestTuple = describe "Record encode NestTuple"
  [ test "1" (\_ -> equalHack "[[],[[],[]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([],([],[]))))))
  , test "2" (\_ -> equalHack "[[-2,2],[[2,1],[2,0]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-2,2],([2,1],[2,0]))))))
  , test "3" (\_ -> equalHack "[[],[[-1,2,1,1],[2,-1,3,-1]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([],([-1,2,1,1],[2,-1,3,-1]))))))
  , test "4" (\_ -> equalHack "[[6],[[0,2],[]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([6],([0,2],[]))))))
  , test "5" (\_ -> equalHack "[[1,4,-4,4,4,-5],[[7,6,4,-2,7,2,-3,6],[-5,-7,-1,-7,3,4,3,-8]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([1,4,-4,4,4,-5],([7,6,4,-2,7,2,-3,6],[-5,-7,-1,-7,3,4,3,-8]))))))
  , test "6" (\_ -> equalHack "[[3,-1,-8,-6,-8],[[-6,5,8],[-4]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([3,-1,-8,-6,-8],([-6,5,8],[-4]))))))
  , test "7" (\_ -> equalHack "[[-4,-1,10,0,-3,-4,-3,8,-1,-3,-9,9],[[-10,-7,7,-4,-1,-6,11,9,10,-11],[1,-7,-6,0,5,-1]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-4,-1,10,0,-3,-4,-3,8,-1,-3,-9,9],([-10,-7,7,-4,-1,-6,11,9,10,-11],[1,-7,-6,0,5,-1]))))))
  , test "8" (\_ -> equalHack "[[-9,11,13,0,5,-12,-8],[[2,11,4,-9,12,-3,-6,-12,-7,3,-6,0],[9,1,0]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-9,11,13,0,5,-12,-8],([2,11,4,-9,12,-3,-6,-12,-7,3,-6,0],[9,1,0]))))))
  , test "9" (\_ -> equalHack "[[-7,11,12,12,10,-1,10,5,3,11,-3,9,9,-10],[[-9,4,-7,-7,4,10,12,-16,-16,15,-5,-2,1,-2,-1],[12,15,10,-7,9,-10,-8,5,-16,5]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-7,11,12,12,10,-1,10,5,3,11,-3,9,9,-10],([-9,4,-7,-7,4,10,12,-16,-16,15,-5,-2,1,-2,-1],[12,15,10,-7,9,-10,-8,5,-16,5]))))))
  , test "10" (\_ -> equalHack "[[-5,2,11,-3,2,16,9,16,13,13,-14,13,-9,-10,-6,16,-4],[[14,10,9,-9,16,2,-9,-14,11,4,-2,2,-7,-17,-13,7],[10,10,-6,9,-7,2,4,9,-7,10,-18,-12]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-5,2,11,-3,2,16,9,16,13,13,-14,13,-9,-10,-6,16,-4],([14,10,9,-9,16,2,-9,-14,11,4,-2,2,-7,-17,-13,7],[10,10,-6,9,-7,2,4,9,-7,10,-18,-12]))))))
  , test "11" (\_ -> equalHack "[[-6,9,11,6,-13,12,18],[[5,-13,11,4,-12,-3,10,-17,14,18],[-13,5,-12,-20,-9,2,12,0]]]"(Json.Encode.encode 0 (jsonEncRecordNestTuple(Json.Encode.list Json.Encode.int) (RecordNestTuple ([-6,9,11,6,-13,12,18],([5,-13,11,4,-12,-3,10,-17,14,18],[-13,5,-12,-20,-9,2,12,0]))))))
  ]

simpleEncode01 : Test
simpleEncode01 = describe "Simple encode 01"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 []))))
  , test "2" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-1]))))
  , test "3" (\_ -> equalHack "[3,2,2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [3,2,2]))))
  , test "4" (\_ -> equalHack "[6,-4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [6,-4]))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 []))))
  , test "6" (\_ -> equalHack "[8,4,-4,8,-10,-4]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [8,4,-4,8,-10,-4]))))
  , test "7" (\_ -> equalHack "[9,-11,-11,-9]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [9,-11,-11,-9]))))
  , test "8" (\_ -> equalHack "[3,-14,5,-9,-10,-11]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [3,-14,5,-9,-10,-11]))))
  , test "9" (\_ -> equalHack "[-2,-1,7,9,-16,-2,15,-3,-8,12]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-2,-1,7,9,-16,-2,15,-3,-8,12]))))
  , test "10" (\_ -> equalHack "[-12,-6,15,11,9,-3,17,-3,6,-6,-17,0,12,-14,13,-9]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-12,-6,15,11,9,-3,17,-3,6,-6,-17,0,12,-14,13,-9]))))
  , test "11" (\_ -> equalHack "[11,14,19,-2]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [11,14,19,-2]))))
  ]

simpleEncode02 : Test
simpleEncode02 = describe "Simple encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "2" (\_ -> equalHack "[1]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [1]))))
  , test "3" (\_ -> equalHack "[-2,1]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-2,1]))))
  , test "4" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [2]))))
  , test "5" (\_ -> equalHack "[-7,5,1,8,5,-5,4]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-7,5,1,8,5,-5,4]))))
  , test "6" (\_ -> equalHack "[-4,-7]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-4,-7]))))
  , test "7" (\_ -> equalHack "[-1,7,-5]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,7,-5]))))
  , test "8" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "9" (\_ -> equalHack "[16,-1,-16,6,0]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [16,-1,-16,6,0]))))
  , test "10" (\_ -> equalHack "[-10,8,4,-9,2,17,-1]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-10,8,4,-9,2,17,-1]))))
  , test "11" (\_ -> equalHack "[11,20,-19,-3,-9,18,-5,-17,12,15,6,5,8,-10,-14,-12,1,5,17]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [11,20,-19,-3,-9,18,-5,-17,12,15,6,5,8,-10,-14,-12,1,5,17]))))
  ]

simpleEncode03 : Test
simpleEncode03 = describe "Simple encode 03"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "2" (\_ -> equalHack "[-2,-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-2,-1]))))
  , test "3" (\_ -> equalHack "[-2,1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-2,1]))))
  , test "4" (\_ -> equalHack "[-4,-2,-2,1,-4]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-4,-2,-2,1,-4]))))
  , test "5" (\_ -> equalHack "[5,-5,-7,3,-1,-2,8]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [5,-5,-7,3,-1,-2,8]))))
  , test "6" (\_ -> equalHack "[3,6,2,-2,3,-5,0,-2,-2]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [3,6,2,-2,3,-5,0,-2,-2]))))
  , test "7" (\_ -> equalHack "[10,11,10,0,-10,10]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [10,11,10,0,-10,10]))))
  , test "8" (\_ -> equalHack "[7,12,10,4,7,-4,-12,14,4,-10,3,-3,-14,-3]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [7,12,10,4,7,-4,-12,14,4,-10,3,-3,-14,-3]))))
  , test "9" (\_ -> equalHack "[-9,12,-11,15,-10,3,6,-14,-1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-9,12,-11,15,-10,3,6,-14,-1]))))
  , test "10" (\_ -> equalHack "[-5,9,5,-17,-7]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-5,9,5,-17,-7]))))
  , test "11" (\_ -> equalHack "[18,7,-1,20,4,-10,-17,7,-3,3,5,-7,15,-16,-1,5,3,-13]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [18,7,-1,20,4,-10,-17,7,-3,3,5,-7,15,-16,-1,5,3,-13]))))
  ]

simpleEncode04 : Test
simpleEncode04 = describe "Simple encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "2" (\_ -> equalHack "[-2,1]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-2,1]))))
  , test "3" (\_ -> equalHack "[-4,2]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-4,2]))))
  , test "4" (\_ -> equalHack "[6,-6]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [6,-6]))))
  , test "5" (\_ -> equalHack "[6,-3,3,7,8,-2,-3]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [6,-3,3,7,8,-2,-3]))))
  , test "6" (\_ -> equalHack "[10]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [10]))))
  , test "7" (\_ -> equalHack "[9,-6,4,-2,7,12,1,-3,-7]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [9,-6,4,-2,7,12,1,-3,-7]))))
  , test "8" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "9" (\_ -> equalHack "[7,4,-8,-14]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [7,4,-8,-14]))))
  , test "10" (\_ -> equalHack "[14,15,10,9,8,15,3,13,-5,-7,15,-9,15]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [14,15,10,9,8,15,3,13,-5,-7,15,-9,15]))))
  , test "11" (\_ -> equalHack "[5,5,13,16,2,2,1,10,-2,-2,1,4,4,-13,19,-5,10,6,3,-5]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [5,5,13,16,2,2,1,10,-2,-2,1,4,4,-13,19,-5,10,6,3,-5]))))
  ]

simpleDecode01 : Test
simpleDecode01 = describe "Simple decode 01"
  [ test "1" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple01 [-1])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-1]"))
  , test "3" (\_ -> equal (Ok (Simple01 [3,2,2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[3,2,2]"))
  , test "4" (\_ -> equal (Ok (Simple01 [6,-4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[6,-4]"))
  , test "5" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "6" (\_ -> equal (Ok (Simple01 [8,4,-4,8,-10,-4])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[8,4,-4,8,-10,-4]"))
  , test "7" (\_ -> equal (Ok (Simple01 [9,-11,-11,-9])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[9,-11,-11,-9]"))
  , test "8" (\_ -> equal (Ok (Simple01 [3,-14,5,-9,-10,-11])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[3,-14,5,-9,-10,-11]"))
  , test "9" (\_ -> equal (Ok (Simple01 [-2,-1,7,9,-16,-2,15,-3,-8,12])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-2,-1,7,9,-16,-2,15,-3,-8,12]"))
  , test "10" (\_ -> equal (Ok (Simple01 [-12,-6,15,11,9,-3,17,-3,6,-6,-17,0,12,-14,13,-9])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-12,-6,15,11,9,-3,17,-3,6,-6,-17,0,12,-14,13,-9]"))
  , test "11" (\_ -> equal (Ok (Simple01 [11,14,19,-2])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[11,14,19,-2]"))
  ]

simpleDecode02 : Test
simpleDecode02 = describe "Simple decode 02"
  [ test "1" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple02 [1])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[1]"))
  , test "3" (\_ -> equal (Ok (Simple02 [-2,1])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-2,1]"))
  , test "4" (\_ -> equal (Ok (Simple02 [2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "5" (\_ -> equal (Ok (Simple02 [-7,5,1,8,5,-5,4])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-7,5,1,8,5,-5,4]"))
  , test "6" (\_ -> equal (Ok (Simple02 [-4,-7])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-4,-7]"))
  , test "7" (\_ -> equal (Ok (Simple02 [-1,7,-5])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,7,-5]"))
  , test "8" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "9" (\_ -> equal (Ok (Simple02 [16,-1,-16,6,0])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[16,-1,-16,6,0]"))
  , test "10" (\_ -> equal (Ok (Simple02 [-10,8,4,-9,2,17,-1])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-10,8,4,-9,2,17,-1]"))
  , test "11" (\_ -> equal (Ok (Simple02 [11,20,-19,-3,-9,18,-5,-17,12,15,6,5,8,-10,-14,-12,1,5,17])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[11,20,-19,-3,-9,18,-5,-17,12,15,6,5,8,-10,-14,-12,1,5,17]"))
  ]

simpleDecode03 : Test
simpleDecode03 = describe "Simple decode 03"
  [ test "1" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple03 [-2,-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-2,-1]"))
  , test "3" (\_ -> equal (Ok (Simple03 [-2,1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-2,1]"))
  , test "4" (\_ -> equal (Ok (Simple03 [-4,-2,-2,1,-4])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-4,-2,-2,1,-4]"))
  , test "5" (\_ -> equal (Ok (Simple03 [5,-5,-7,3,-1,-2,8])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[5,-5,-7,3,-1,-2,8]"))
  , test "6" (\_ -> equal (Ok (Simple03 [3,6,2,-2,3,-5,0,-2,-2])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[3,6,2,-2,3,-5,0,-2,-2]"))
  , test "7" (\_ -> equal (Ok (Simple03 [10,11,10,0,-10,10])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[10,11,10,0,-10,10]"))
  , test "8" (\_ -> equal (Ok (Simple03 [7,12,10,4,7,-4,-12,14,4,-10,3,-3,-14,-3])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[7,12,10,4,7,-4,-12,14,4,-10,3,-3,-14,-3]"))
  , test "9" (\_ -> equal (Ok (Simple03 [-9,12,-11,15,-10,3,6,-14,-1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-9,12,-11,15,-10,3,6,-14,-1]"))
  , test "10" (\_ -> equal (Ok (Simple03 [-5,9,5,-17,-7])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-5,9,5,-17,-7]"))
  , test "11" (\_ -> equal (Ok (Simple03 [18,7,-1,20,4,-10,-17,7,-3,3,5,-7,15,-16,-1,5,3,-13])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[18,7,-1,20,4,-10,-17,7,-3,3,5,-7,15,-16,-1,5,3,-13]"))
  ]

simpleDecode04 : Test
simpleDecode04 = describe "Simple decode 04"
  [ test "1" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple04 [-2,1])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-2,1]"))
  , test "3" (\_ -> equal (Ok (Simple04 [-4,2])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-4,2]"))
  , test "4" (\_ -> equal (Ok (Simple04 [6,-6])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[6,-6]"))
  , test "5" (\_ -> equal (Ok (Simple04 [6,-3,3,7,8,-2,-3])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[6,-3,3,7,8,-2,-3]"))
  , test "6" (\_ -> equal (Ok (Simple04 [10])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[10]"))
  , test "7" (\_ -> equal (Ok (Simple04 [9,-6,4,-2,7,12,1,-3,-7])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[9,-6,4,-2,7,12,1,-3,-7]"))
  , test "8" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "9" (\_ -> equal (Ok (Simple04 [7,4,-8,-14])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[7,4,-8,-14]"))
  , test "10" (\_ -> equal (Ok (Simple04 [14,15,10,9,8,15,3,13,-5,-7,15,-9,15])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[14,15,10,9,8,15,3,13,-5,-7,15,-9,15]"))
  , test "11" (\_ -> equal (Ok (Simple04 [5,5,13,16,2,2,1,10,-2,-2,1,4,4,-13,19,-5,10,6,3,-5])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[5,5,13,16,2,2,1,10,-2,-2,1,4,4,-13,19,-5,10,6,3,-5]"))
  ]

simplerecordEncode01 : Test
simplerecordEncode01 = describe "SimpleRecord encode 01"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[0,-1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [0,-1]}))))
  , test "3" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "4" (\_ -> equalHack "{\"qux\":[3,-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [3,-2]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[-6,-5,7,-5,-6,2,7,1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-6,-5,7,-5,-6,2,7,1]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[10]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [10]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[1,6,-6,-6,12,2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [1,6,-6,-6,12,2]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[-5,13,-1,12]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-5,13,-1,12]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-2,-1,-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-2,-1,-2]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[12,5,-4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [12,5,-4]}))))
  ]

simplerecordEncode02 : Test
simplerecordEncode02 = describe "SimpleRecord encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "3" (\_ -> equalHack "[-2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-2]}))))
  , test "4" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [2]}))))
  , test "5" (\_ -> equalHack "[-5,3,0,6,-2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-5,3,0,6,-2]}))))
  , test "6" (\_ -> equalHack "[2,10,6]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [2,10,6]}))))
  , test "7" (\_ -> equalHack "[-8,-1,-5,-5,5,-9,-11,-2,2,4,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-8,-1,-5,-5,5,-9,-11,-2,2,4,-7]}))))
  , test "8" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "9" (\_ -> equalHack "[-3,-2,11,-15,-9,11,10,14,-13,7,12,-10,5]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-3,-2,11,-15,-9,11,10,14,-13,7,12,-10,5]}))))
  , test "10" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "11" (\_ -> equalHack "[-1,-15,-3,-1,20,7,-13,8,17,13,-3,-5,-12]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-1,-15,-3,-1,20,7,-13,8,17,13,-3,-5,-12]}))))
  ]

simplerecordEncode03 : Test
simplerecordEncode03 = describe "SimpleRecord encode 03"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[-1,-1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-1,-1]}))))
  , test "3" (\_ -> equalHack "{\"qux\":[1,4,4]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [1,4,4]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [0]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[3,3,8,2,-7]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [3,3,8,2,-7]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[8,2,-8,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [8,2,-8,-5]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[-8,-4,8]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-8,-4,8]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[2,-11,-1,2,-2,7,-12,13]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [2,-11,-1,2,-2,7,-12,13]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[-16,-1,2,0,-16,-15,-15,6,-9,-12,8]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-16,-1,2,0,-16,-15,-15,6,-9,-12,8]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-4,12,7,18,4,-9,4,18,-1,3,16]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-4,12,7,18,4,-9,4,18,-1,3,16]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  ]

simplerecordEncode04 : Test
simplerecordEncode04 = describe "SimpleRecord encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "2" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "3" (\_ -> equalHack "[-4,3]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-4,3]}))))
  , test "4" (\_ -> equalHack "[1,6,-4,4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [1,6,-4,4]}))))
  , test "5" (\_ -> equalHack "[-6,2,-5,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-6,2,-5,-7]}))))
  , test "6" (\_ -> equalHack "[9,-8,-2,-1,-4,-3,7,1,8,-7]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [9,-8,-2,-1,-4,-3,7,1,8,-7]}))))
  , test "7" (\_ -> equalHack "[12,2,10,11]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [12,2,10,11]}))))
  , test "8" (\_ -> equalHack "[-1,6,-10,-12]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-1,6,-10,-12]}))))
  , test "9" (\_ -> equalHack "[-6,-7,-3,6,3,-7,-10,-15,15,6,-15,10,-2,14,6]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-6,-7,-3,6,3,-7,-10,-15,15,6,-15,10,-2,14,6]}))))
  , test "10" (\_ -> equalHack "[-17,-17,-17,-16,-17,0,14,-5,-9]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-17,-17,-17,-16,-17,0,14,-5,-9]}))))
  , test "11" (\_ -> equalHack "[8,-9,-18,17,8,18,-9,-14,7,7,-19,20]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [8,-9,-18,17,8,18,-9,-14,7,7,-19,20]}))))
  ]

simplerecordDecode01 : Test
simplerecordDecode01 = describe "SimpleRecord decode 01"
  [ test "1" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord01 {qux = [0,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,-1]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord01 {qux = [3,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[3,-2]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord01 {qux = [-6,-5,7,-5,-6,2,7,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-6,-5,7,-5,-6,2,7,1]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord01 {qux = [10]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[10]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord01 {qux = [1,6,-6,-6,12,2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[1,6,-6,-6,12,2]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord01 {qux = [-5,13,-1,12]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-5,13,-1,12]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord01 {qux = [-2,-1,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-2,-1,-2]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord01 {qux = [12,5,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[12,5,-4]}"))
  ]

simplerecordDecode02 : Test
simplerecordDecode02 = describe "SimpleRecord decode 02"
  [ test "1" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord02 {qux = [-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-2]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord02 {qux = [2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord02 {qux = [-5,3,0,6,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-5,3,0,6,-2]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord02 {qux = [2,10,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[2,10,6]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord02 {qux = [-8,-1,-5,-5,5,-9,-11,-2,2,4,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-8,-1,-5,-5,5,-9,-11,-2,2,4,-7]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord02 {qux = [-3,-2,11,-15,-9,11,10,14,-13,7,12,-10,5]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-3,-2,11,-15,-9,11,10,14,-13,7,12,-10,5]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord02 {qux = [-1,-15,-3,-1,20,7,-13,8,17,13,-3,-5,-12]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-1,-15,-3,-1,20,7,-13,8,17,13,-3,-5,-12]"))
  ]

simplerecordDecode03 : Test
simplerecordDecode03 = describe "SimpleRecord decode 03"
  [ test "1" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord03 {qux = [-1,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,-1]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord03 {qux = [1,4,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[1,4,4]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord03 {qux = [0]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord03 {qux = [3,3,8,2,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[3,3,8,2,-7]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord03 {qux = [8,2,-8,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[8,2,-8,-5]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord03 {qux = [-8,-4,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-8,-4,8]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord03 {qux = [2,-11,-1,2,-2,7,-12,13]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[2,-11,-1,2,-2,7,-12,13]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord03 {qux = [-16,-1,2,0,-16,-15,-15,6,-9,-12,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-16,-1,2,0,-16,-15,-15,6,-9,-12,8]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord03 {qux = [-4,12,7,18,4,-9,4,18,-1,3,16]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-4,12,7,18,4,-9,4,18,-1,3,16]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  ]

simplerecordDecode04 : Test
simplerecordDecode04 = describe "SimpleRecord decode 04"
  [ test "1" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord04 {qux = [-4,3]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-4,3]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord04 {qux = [1,6,-4,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[1,6,-4,4]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord04 {qux = [-6,2,-5,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-6,2,-5,-7]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord04 {qux = [9,-8,-2,-1,-4,-3,7,1,8,-7]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[9,-8,-2,-1,-4,-3,7,1,8,-7]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord04 {qux = [12,2,10,11]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[12,2,10,11]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord04 {qux = [-1,6,-10,-12]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-1,6,-10,-12]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord04 {qux = [-6,-7,-3,6,3,-7,-10,-15,15,6,-15,10,-2,14,6]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-6,-7,-3,6,3,-7,-10,-15,15,6,-15,10,-2,14,6]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord04 {qux = [-17,-17,-17,-16,-17,0,14,-5,-9]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-17,-17,-17,-16,-17,0,14,-5,-9]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord04 {qux = [8,-9,-18,17,8,18,-9,-14,7,7,-19,20]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[8,-9,-18,17,8,18,-9,-14,7,7,-19,20]"))
  ]

sumEncodeUntagged : Test
sumEncodeUntagged = describe "Sum encode Untagged"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList []))))
  , test "2" (\_ -> equalHack "-1"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-1)))))
  , test "3" (\_ -> equalHack "[3,4]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [3,4]))))
  , test "4" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "5" (\_ -> equalHack "-1"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-1)))))
  , test "6" (\_ -> equalHack "-4"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-4)))))
  , test "7" (\_ -> equalHack "0"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 0))))
  , test "8" (\_ -> equalHack "[13,-13,4,3,-14]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [13,-13,4,3,-14]))))
  , test "9" (\_ -> equalHack "[-8,-7,7,-15,-3]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [-8,-7,7,-15,-3]))))
  , test "10" (\_ -> equalHack "[5,-10,-8,-6,-4,-1,14,9,-7,4,-11,-12,13,15,-14,-14,9]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [5,-10,-8,-6,-4,-1,14,9,-7,4,-11,-12,13,15,-14,-14,9]))))
  , test "11" (\_ -> equalHack "10"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 10))))
  ]

sumDecodeUntagged : Test
sumDecodeUntagged = describe "Sum decode Untagged"
  [ test "1" (\_ -> equal (Ok (SMList [])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SMInt (-1))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-1"))
  , test "3" (\_ -> equal (Ok (SMList [3,4])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[3,4]"))
  , test "4" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "5" (\_ -> equal (Ok (SMInt (-1))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-1"))
  , test "6" (\_ -> equal (Ok (SMInt (-4))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-4"))
  , test "7" (\_ -> equal (Ok (SMInt 0)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "0"))
  , test "8" (\_ -> equal (Ok (SMList [13,-13,4,3,-14])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[13,-13,4,3,-14]"))
  , test "9" (\_ -> equal (Ok (SMList [-8,-7,7,-15,-3])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-8,-7,7,-15,-3]"))
  , test "10" (\_ -> equal (Ok (SMList [5,-10,-8,-6,-4,-1,14,9,-7,4,-11,-12,13,15,-14,-14,9])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[5,-10,-8,-6,-4,-1,14,9,-7,4,-11,-12,13,15,-14,-14,9]"))
  , test "11" (\_ -> equal (Ok (SMInt 10)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "10"))
  ]

sumEncodeIncludeUnit : Test
sumEncodeIncludeUnit = describe "Sum encode IncludeUnit"
  [ test "1" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitZero\"}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitZero))))
  , test "3" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[4,-1,-3,-3],[]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [4,-1,-3,-3] []))))
  , test "4" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[0,-4,-6,2,-4]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [] [0,-4,-6,2,-4]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-1,-1,0,1,-2]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-1,-1,0,1,-2]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[6,-3,-1,10,-4,-6,8,1],[-9,-10,-9,-3,1,10]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [6,-3,-1,10,-4,-6,8,1] [-9,-10,-9,-3,1,10]))))
  , test "7" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[11,7,-11,-6,4,2,-1,9,2,-2,-8,4],[2]]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitTwo [11,7,-11,-6,4,2,-1,9,2,-2,-8,4] [2]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[4,-12]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [4,-12]))))
  , test "9" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,-5,-3,0,1,7,10,1,13]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-3,-5,-3,0,1,7,10,1,13]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-13,17,10,-9,17,12,11]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [-13,17,10,-9,17,12,11]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"SumIncludeUnitOne\",\"content\":[3,6,-9,7,1]}"(Json.Encode.encode 0 (jsonEncSumIncludeUnit(Json.Encode.list Json.Encode.int) (SumIncludeUnitOne [3,6,-9,7,1]))))
  ]

sumDecodeIncludeUnit : Test
sumDecodeIncludeUnit = describe "Sum decode IncludeUnit"
  [ test "1" (\_ -> equal (Ok (SumIncludeUnitTwo [] [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (SumIncludeUnitZero)) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitZero\"}"))
  , test "3" (\_ -> equal (Ok (SumIncludeUnitTwo [4,-1,-3,-3] [])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[4,-1,-3,-3],[]]}"))
  , test "4" (\_ -> equal (Ok (SumIncludeUnitTwo [] [0,-4,-6,2,-4])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[],[0,-4,-6,2,-4]]}"))
  , test "5" (\_ -> equal (Ok (SumIncludeUnitOne [-1,-1,0,1,-2])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-1,-1,0,1,-2]}"))
  , test "6" (\_ -> equal (Ok (SumIncludeUnitTwo [6,-3,-1,10,-4,-6,8,1] [-9,-10,-9,-3,1,10])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[6,-3,-1,10,-4,-6,8,1],[-9,-10,-9,-3,1,10]]}"))
  , test "7" (\_ -> equal (Ok (SumIncludeUnitTwo [11,7,-11,-6,4,2,-1,9,2,-2,-8,4] [2])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitTwo\",\"content\":[[11,7,-11,-6,4,2,-1,9,2,-2,-8,4],[2]]}"))
  , test "8" (\_ -> equal (Ok (SumIncludeUnitOne [4,-12])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[4,-12]}"))
  , test "9" (\_ -> equal (Ok (SumIncludeUnitOne [-3,-5,-3,0,1,7,10,1,13])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-3,-5,-3,0,1,7,10,1,13]}"))
  , test "10" (\_ -> equal (Ok (SumIncludeUnitOne [-13,17,10,-9,17,12,11])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[-13,17,10,-9,17,12,11]}"))
  , test "11" (\_ -> equal (Ok (SumIncludeUnitOne [3,6,-9,7,1])) (Json.Decode.decodeString (jsonDecSumIncludeUnit (Json.Decode.list Json.Decode.int)) "{\"tag\":\"SumIncludeUnitOne\",\"content\":[3,6,-9,7,1]}"))
  ]

ntDecode1 : Test
ntDecode1 = describe "NT decode 1"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "2" (\_ -> equal (Ok ([2,1])) (Json.Decode.decodeString jsonDecNT1 "[2,1]"))
  , test "3" (\_ -> equal (Ok ([-2])) (Json.Decode.decodeString jsonDecNT1 "[-2]"))
  , test "4" (\_ -> equal (Ok ([5,2,4,1,-5])) (Json.Decode.decodeString jsonDecNT1 "[5,2,4,1,-5]"))
  , test "5" (\_ -> equal (Ok ([-1,1,7,-3])) (Json.Decode.decodeString jsonDecNT1 "[-1,1,7,-3]"))
  , test "6" (\_ -> equal (Ok ([-9,0])) (Json.Decode.decodeString jsonDecNT1 "[-9,0]"))
  , test "7" (\_ -> equal (Ok ([4,12,-4,-1,-12,-2])) (Json.Decode.decodeString jsonDecNT1 "[4,12,-4,-1,-12,-2]"))
  , test "8" (\_ -> equal (Ok ([0,-6,-9,8,-8,4,-7])) (Json.Decode.decodeString jsonDecNT1 "[0,-6,-9,8,-8,4,-7]"))
  , test "9" (\_ -> equal (Ok ([3,-8,12,5,-16])) (Json.Decode.decodeString jsonDecNT1 "[3,-8,12,5,-16]"))
  , test "10" (\_ -> equal (Ok ([-7,5,10,-11,-5,2,-17])) (Json.Decode.decodeString jsonDecNT1 "[-7,5,10,-11,-5,2,-17]"))
  , test "11" (\_ -> equal (Ok ([-3,20,7,-8,-1,-6,-2,-18,13,9])) (Json.Decode.decodeString jsonDecNT1 "[-3,20,7,-8,-1,-6,-2,-18,13,9]"))
  ]

ntEncode1 : Test
ntEncode1 = describe "NT encode 1"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "2" (\_ -> equalHack "[2,1]"(Json.Encode.encode 0 (jsonEncNT1 ([2,1]))))
  , test "3" (\_ -> equalHack "[-2]"(Json.Encode.encode 0 (jsonEncNT1 ([-2]))))
  , test "4" (\_ -> equalHack "[5,2,4,1,-5]"(Json.Encode.encode 0 (jsonEncNT1 ([5,2,4,1,-5]))))
  , test "5" (\_ -> equalHack "[-1,1,7,-3]"(Json.Encode.encode 0 (jsonEncNT1 ([-1,1,7,-3]))))
  , test "6" (\_ -> equalHack "[-9,0]"(Json.Encode.encode 0 (jsonEncNT1 ([-9,0]))))
  , test "7" (\_ -> equalHack "[4,12,-4,-1,-12,-2]"(Json.Encode.encode 0 (jsonEncNT1 ([4,12,-4,-1,-12,-2]))))
  , test "8" (\_ -> equalHack "[0,-6,-9,8,-8,4,-7]"(Json.Encode.encode 0 (jsonEncNT1 ([0,-6,-9,8,-8,4,-7]))))
  , test "9" (\_ -> equalHack "[3,-8,12,5,-16]"(Json.Encode.encode 0 (jsonEncNT1 ([3,-8,12,5,-16]))))
  , test "10" (\_ -> equalHack "[-7,5,10,-11,-5,2,-17]"(Json.Encode.encode 0 (jsonEncNT1 ([-7,5,10,-11,-5,2,-17]))))
  , test "11" (\_ -> equalHack "[-3,20,7,-8,-1,-6,-2,-18,13,9]"(Json.Encode.encode 0 (jsonEncNT1 ([-3,20,7,-8,-1,-6,-2,-18,13,9]))))
  ]

ntDecode2 : Test
ntDecode2 = describe "NT decode 2"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "2" (\_ -> equal (Ok ([-2,0])) (Json.Decode.decodeString jsonDecNT2 "[-2,0]"))
  , test "3" (\_ -> equal (Ok ([-4,-2])) (Json.Decode.decodeString jsonDecNT2 "[-4,-2]"))
  , test "4" (\_ -> equal (Ok ([4,5,5])) (Json.Decode.decodeString jsonDecNT2 "[4,5,5]"))
  , test "5" (\_ -> equal (Ok ([3,1,3,0,6,4,-2,6])) (Json.Decode.decodeString jsonDecNT2 "[3,1,3,0,6,4,-2,6]"))
  , test "6" (\_ -> equal (Ok ([-1,-5,-1,-4])) (Json.Decode.decodeString jsonDecNT2 "[-1,-5,-1,-4]"))
  , test "7" (\_ -> equal (Ok ([8,-7,-2,2])) (Json.Decode.decodeString jsonDecNT2 "[8,-7,-2,2]"))
  , test "8" (\_ -> equal (Ok ([-14,11,-5,14,12,10,14,11,14,-13,-9,-1])) (Json.Decode.decodeString jsonDecNT2 "[-14,11,-5,14,12,10,14,11,14,-13,-9,-1]"))
  , test "9" (\_ -> equal (Ok ([-12,-2,-13,-4])) (Json.Decode.decodeString jsonDecNT2 "[-12,-2,-13,-4]"))
  , test "10" (\_ -> equal (Ok ([2])) (Json.Decode.decodeString jsonDecNT2 "[2]"))
  , test "11" (\_ -> equal (Ok ([15,2,8,-20,0,-13,13,-1,5])) (Json.Decode.decodeString jsonDecNT2 "[15,2,8,-20,0,-13,13,-1,5]"))
  ]

ntEncode2 : Test
ntEncode2 = describe "NT encode 2"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "2" (\_ -> equalHack "[-2,0]"(Json.Encode.encode 0 (jsonEncNT2 ([-2,0]))))
  , test "3" (\_ -> equalHack "[-4,-2]"(Json.Encode.encode 0 (jsonEncNT2 ([-4,-2]))))
  , test "4" (\_ -> equalHack "[4,5,5]"(Json.Encode.encode 0 (jsonEncNT2 ([4,5,5]))))
  , test "5" (\_ -> equalHack "[3,1,3,0,6,4,-2,6]"(Json.Encode.encode 0 (jsonEncNT2 ([3,1,3,0,6,4,-2,6]))))
  , test "6" (\_ -> equalHack "[-1,-5,-1,-4]"(Json.Encode.encode 0 (jsonEncNT2 ([-1,-5,-1,-4]))))
  , test "7" (\_ -> equalHack "[8,-7,-2,2]"(Json.Encode.encode 0 (jsonEncNT2 ([8,-7,-2,2]))))
  , test "8" (\_ -> equalHack "[-14,11,-5,14,12,10,14,11,14,-13,-9,-1]"(Json.Encode.encode 0 (jsonEncNT2 ([-14,11,-5,14,12,10,14,11,14,-13,-9,-1]))))
  , test "9" (\_ -> equalHack "[-12,-2,-13,-4]"(Json.Encode.encode 0 (jsonEncNT2 ([-12,-2,-13,-4]))))
  , test "10" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncNT2 ([2]))))
  , test "11" (\_ -> equalHack "[15,2,8,-20,0,-13,13,-1,5]"(Json.Encode.encode 0 (jsonEncNT2 ([15,2,8,-20,0,-13,13,-1,5]))))
  ]

ntDecode3 : Test
ntDecode3 = describe "NT decode 3"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "2" (\_ -> equal (Ok ([2,2])) (Json.Decode.decodeString jsonDecNT3 "[2,2]"))
  , test "3" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "4" (\_ -> equal (Ok ([3,-6,1,-6,-4,1])) (Json.Decode.decodeString jsonDecNT3 "[3,-6,1,-6,-4,1]"))
  , test "5" (\_ -> equal (Ok ([2,-8,-1,1,-7,3])) (Json.Decode.decodeString jsonDecNT3 "[2,-8,-1,1,-7,3]"))
  , test "6" (\_ -> equal (Ok ([-3,-9,5,-4])) (Json.Decode.decodeString jsonDecNT3 "[-3,-9,5,-4]"))
  , test "7" (\_ -> equal (Ok ([-3,-1,4,1])) (Json.Decode.decodeString jsonDecNT3 "[-3,-1,4,1]"))
  , test "8" (\_ -> equal (Ok ([11,14])) (Json.Decode.decodeString jsonDecNT3 "[11,14]"))
  , test "9" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "10" (\_ -> equal (Ok ([-1])) (Json.Decode.decodeString jsonDecNT3 "[-1]"))
  , test "11" (\_ -> equal (Ok ([7,1,-4])) (Json.Decode.decodeString jsonDecNT3 "[7,1,-4]"))
  ]

ntEncode3 : Test
ntEncode3 = describe "NT encode 3"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "2" (\_ -> equalHack "[2,2]"(Json.Encode.encode 0 (jsonEncNT3 ([2,2]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "4" (\_ -> equalHack "[3,-6,1,-6,-4,1]"(Json.Encode.encode 0 (jsonEncNT3 ([3,-6,1,-6,-4,1]))))
  , test "5" (\_ -> equalHack "[2,-8,-1,1,-7,3]"(Json.Encode.encode 0 (jsonEncNT3 ([2,-8,-1,1,-7,3]))))
  , test "6" (\_ -> equalHack "[-3,-9,5,-4]"(Json.Encode.encode 0 (jsonEncNT3 ([-3,-9,5,-4]))))
  , test "7" (\_ -> equalHack "[-3,-1,4,1]"(Json.Encode.encode 0 (jsonEncNT3 ([-3,-1,4,1]))))
  , test "8" (\_ -> equalHack "[11,14]"(Json.Encode.encode 0 (jsonEncNT3 ([11,14]))))
  , test "9" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "10" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncNT3 ([-1]))))
  , test "11" (\_ -> equalHack "[7,1,-4]"(Json.Encode.encode 0 (jsonEncNT3 ([7,1,-4]))))
  ]

ntDecode4 : Test
ntDecode4 = describe "NT decode 4"
  [ test "1" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "3" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "4" (\_ -> equal (Ok (NT4 {foo = [-3,-3,4,2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-3,-3,4,2]}"))
  , test "5" (\_ -> equal (Ok (NT4 {foo = [-4,-6,1,3,-6,4,8,1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-4,-6,1,3,-6,4,8,1]}"))
  , test "6" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "7" (\_ -> equal (Ok (NT4 {foo = [0,1,-1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[0,1,-1]}"))
  , test "8" (\_ -> equal (Ok (NT4 {foo = [10,9,-1,-2,2]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[10,9,-1,-2,2]}"))
  , test "9" (\_ -> equal (Ok (NT4 {foo = [-1,12,-16,15,-11,-6,-2,14,-15,-14,-7]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-1,12,-16,15,-11,-6,-2,14,-15,-14,-7]}"))
  , test "10" (\_ -> equal (Ok (NT4 {foo = [-1,-12,-8,17,5,-13,9,-5,2,-4,-4]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-1,-12,-8,17,5,-13,9,-5,2,-4,-4]}"))
  , test "11" (\_ -> equal (Ok (NT4 {foo = [-7,-15,2,-7,-6,-16,19,-5,-14,14,-11,-2,-12,13,15,9,10,-3]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-7,-15,2,-7,-6,-16,19,-5,-14,14,-11,-2,-12,13,15,9,10,-3]}"))
  ]

ntEncode4 : Test
ntEncode4 = describe "NT encode 4"
  [ test "1" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "2" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "3" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "4" (\_ -> equalHack "{\"foo\":[-3,-3,4,2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-3,-3,4,2]}))))
  , test "5" (\_ -> equalHack "{\"foo\":[-4,-6,1,3,-6,4,8,1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-4,-6,1,3,-6,4,8,1]}))))
  , test "6" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "7" (\_ -> equalHack "{\"foo\":[0,1,-1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [0,1,-1]}))))
  , test "8" (\_ -> equalHack "{\"foo\":[10,9,-1,-2,2]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [10,9,-1,-2,2]}))))
  , test "9" (\_ -> equalHack "{\"foo\":[-1,12,-16,15,-11,-6,-2,14,-15,-14,-7]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-1,12,-16,15,-11,-6,-2,14,-15,-14,-7]}))))
  , test "10" (\_ -> equalHack "{\"foo\":[-1,-12,-8,17,5,-13,9,-5,2,-4,-4]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-1,-12,-8,17,5,-13,9,-5,2,-4,-4]}))))
  , test "11" (\_ -> equalHack "{\"foo\":[-7,-15,2,-7,-6,-16,19,-5,-14,14,-11,-2,-12,13,15,9,10,-3]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-7,-15,2,-7,-6,-16,19,-5,-14,14,-11,-2,-12,13,15,9,10,-3]}))))
  ]

