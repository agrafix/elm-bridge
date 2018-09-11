{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Elm.Derive
import Elm.Module
import Data.Proxy
import Data.Aeson hiding (defaultOptions)
import Data.Aeson.Types (SumEncoding(..))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (sample', oneof, Gen)
import qualified Data.Text as T
import Control.Applicative
import System.Environment
import Data.Char (toLower)
import Data.List (stripPrefix)
import Prelude

data Record1 a = Record1 { _r1foo :: Int, _r1bar :: Maybe Int, _r1baz :: a, _r1qux :: Maybe a } deriving Show
data Record2 a = Record2 { _r2foo :: Int, _r2bar :: Maybe Int, _r2baz :: a, _r2qux :: Maybe a } deriving Show

data Sum01 a = Sum01A a | Sum01B (Maybe a) | Sum01C a a | Sum01D { _s01foo :: a } | Sum01E { _s01bar :: Int, _s01baz :: Int } deriving Show
data Sum02 a = Sum02A a | Sum02B (Maybe a) | Sum02C a a | Sum02D { _s02foo :: a } | Sum02E { _s02bar :: Int, _s02baz :: Int } deriving Show
data Sum03 a = Sum03A a | Sum03B (Maybe a) | Sum03C a a | Sum03D { _s03foo :: a } | Sum03E { _s03bar :: Int, _s03baz :: Int } deriving Show
data Sum04 a = Sum04A a | Sum04B (Maybe a) | Sum04C a a | Sum04D { _s04foo :: a } | Sum04E { _s04bar :: Int, _s04baz :: Int } deriving Show
data Sum05 a = Sum05A a | Sum05B (Maybe a) | Sum05C a a | Sum05D { _s05foo :: a } | Sum05E { _s05bar :: Int, _s05baz :: Int } deriving Show
data Sum06 a = Sum06A a | Sum06B (Maybe a) | Sum06C a a | Sum06D { _s06foo :: a } | Sum06E { _s06bar :: Int, _s06baz :: Int } deriving Show
data Sum07 a = Sum07A a | Sum07B (Maybe a) | Sum07C a a | Sum07D { _s07foo :: a } | Sum07E { _s07bar :: Int, _s07baz :: Int } deriving Show
data Sum08 a = Sum08A a | Sum08B (Maybe a) | Sum08C a a | Sum08D { _s08foo :: a } | Sum08E { _s08bar :: Int, _s08baz :: Int } deriving Show
data Sum09 a = Sum09A a | Sum09B (Maybe a) | Sum09C a a | Sum09D { _s09foo :: a } | Sum09E { _s09bar :: Int, _s09baz :: Int } deriving Show
data Sum10 a = Sum10A a | Sum10B (Maybe a) | Sum10C a a | Sum10D { _s10foo :: a } | Sum10E { _s10bar :: Int, _s10baz :: Int } deriving Show
data Sum11 a = Sum11A a | Sum11B (Maybe a) | Sum11C a a | Sum11D { _s11foo :: a } | Sum11E { _s11bar :: Int, _s11baz :: Int } deriving Show
data Sum12 a = Sum12A a | Sum12B (Maybe a) | Sum12C a a | Sum12D { _s12foo :: a } | Sum12E { _s12bar :: Int, _s12baz :: Int } deriving Show

data Simple01 a = Simple01 a deriving Show
data Simple02 a = Simple02 a deriving Show
data Simple03 a = Simple03 a deriving Show
data Simple04 a = Simple04 a deriving Show
data SimpleRecord01 a = SimpleRecord01 { _s01qux :: a } deriving Show
data SimpleRecord02 a = SimpleRecord02 { _s02qux :: a } deriving Show
data SimpleRecord03 a = SimpleRecord03 { _s03qux :: a } deriving Show
data SimpleRecord04 a = SimpleRecord04 { _s04qux :: a } deriving Show

data SumUntagged a = SMInt Int | SMList a
              deriving Show

newtype NT1 = NT1 [Int] deriving Show
newtype NT2 = NT2 { _nt2foo :: [Int] } deriving Show
newtype NT3 = NT3 [Int] deriving Show
newtype NT4 = NT4 { _nt4foo :: [Int] } deriving Show

extractNT1 :: NT1 -> [Int]
extractNT1 (NT1 x) =x
extractNT2 :: NT2 -> [Int]
extractNT2 (NT2 x) =x
extractNT3 :: NT3 -> [Int]
extractNT3 (NT3 x) =x

dropAll :: String -> String -> String
dropAll needle haystack
  = case stripPrefix needle haystack of
      Just nxt -> dropAll needle nxt
      Nothing -> case haystack of
                   [] -> []
                   (x:xs) -> x : dropAll needle xs


mkDecodeTest :: (Show a, ToJSON a) => String -> String -> String -> [a] -> String
mkDecodeTest pred prefix num elems = unlines (
    [ map toLower pred ++ "Decode" ++ num ++ " : Test"
    , map toLower pred ++ "Decode" ++ num ++ " = describe \"" ++ pred ++ " decode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = pfix ++ "test \"" ++ show n ++ "\" (\\_ -> equal (Ok (" ++ pretty ++ ")) (Json.Decode.decodeString (jsonDec" ++ pred ++ num ++ " (Json.Decode.list Json.Decode.int)) " ++ encoded ++ "))"
        where
            pretty = T.unpack $ T.replace (T.pack (prefix ++ num)) T.empty $ T.pack $ show e
            encoded = show (encode e)
            pfix = if n == 1 then "  [ " else "  , "

mkDecodeTestNT :: ToJSON n => String -> String -> String -> (n -> [Int]) -> [n] -> String
mkDecodeTestNT pred prefix num extract elems = unlines (
    [ map toLower pred ++ "Decode" ++ num ++ " : Test"
    , map toLower pred ++ "Decode" ++ num ++ " = describe \"" ++ pred ++ " decode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = pfix ++ "test \"" ++ show n ++ "\" (\\_ -> equal (Ok (" ++ pretty ++ ")) (Json.Decode.decodeString jsonDec" ++ pred ++ num ++ " " ++ encoded ++ "))"
        where
            pretty = T.unpack $ T.replace (T.pack (prefix ++ num)) T.empty $ T.pack $ show (extract e)
            encoded = show (encode e)
            pfix = if n == 1 then "  [ " else "  , "

mkSumDecodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSumDecodeTest = mkDecodeTest "Sum" "_s"

mkRecordDecodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkRecordDecodeTest = mkDecodeTest "Record" "_r"

mkEncodeTest :: (Show a, ToJSON a) => String -> String -> String -> [a] -> String
mkEncodeTest pred prefix num elems = unlines (
    [ map toLower pred ++ "Encode" ++ num ++ " : Test"
    , map toLower pred ++ "Encode" ++ num ++ " = describe \"" ++ pred ++ " encode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = pfix ++ "test \"" ++ show n ++ "\" (\\_ -> equalHack " ++ encoded ++ "(Json.Encode.encode 0 (jsonEnc" ++ pred ++ num ++ "(Json.Encode.list Json.Encode.int) (" ++ pretty ++ "))))"
        where
            pretty = T.unpack $ T.replace (T.pack (prefix ++ num)) T.empty $ T.pack $ show e
            encoded = show (encode e)
            pfix = if n == 1 then "  [ " else "  , "

mkEncodeTestNT :: (Show a, ToJSON n) => String -> String -> String -> (n -> a) -> [n] -> String
mkEncodeTestNT pred prefix num extract elems = unlines (
    [ map toLower pred ++ "Encode" ++ num ++ " : Test"
    , map toLower pred ++ "Encode" ++ num ++ " = describe \"" ++ pred ++ " encode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = pfix ++ "test \"" ++ show n ++ "\" (\\_ -> equalHack " ++ encoded ++ "(Json.Encode.encode 0 (jsonEnc" ++ pred ++ num ++ " (" ++ pretty ++ "))))"
        where
            pretty = T.unpack $ T.replace (T.pack (prefix ++ num)) T.empty $ T.pack $ show (extract e)
            encoded = show (encode e)
            pfix = if n == 1 then "  [ " else "  , "

mkSumEncodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSumEncodeTest = mkEncodeTest "Sum" "_s"

mkRecordEncodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkRecordEncodeTest = mkEncodeTest "Record" "_r"

mkSimpleRecordDecodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSimpleRecordDecodeTest = mkDecodeTest "SimpleRecord" "_s"

mkSimpleRecordEncodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSimpleRecordEncodeTest = mkEncodeTest "SimpleRecord" "_s"

mkSimpleDecodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSimpleDecodeTest = mkDecodeTest "Simple" "_s"

mkSimpleEncodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSimpleEncodeTest = mkEncodeTest "Simple" "_s"


$(deriveBoth defaultOptions{ fieldLabelModifier = drop 3, omitNothingFields = False } ''Record1)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 3, omitNothingFields = True  } ''Record2)

$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = False, sumEncoding = TaggedObject "tag" "content" } ''Sum01)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = False, sumEncoding = TaggedObject "tag" "content" } ''Sum02)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = True , sumEncoding = TaggedObject "tag" "content" } ''Sum03)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = True , sumEncoding = TaggedObject "tag" "content" } ''Sum04)

$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = False, sumEncoding = ObjectWithSingleField } ''Sum05)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = False, sumEncoding = ObjectWithSingleField } ''Sum06)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = True , sumEncoding = ObjectWithSingleField } ''Sum07)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = True , sumEncoding = ObjectWithSingleField } ''Sum08)

$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = False, sumEncoding = TwoElemArray } ''Sum09)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = False, sumEncoding = TwoElemArray } ''Sum10)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = False, allNullaryToStringTag = True , sumEncoding = TwoElemArray } ''Sum11)
$(deriveBoth defaultOptions{ fieldLabelModifier = drop 4, omitNothingFields = True , allNullaryToStringTag = True , sumEncoding = TwoElemArray } ''Sum12)

$(deriveBoth defaultOptions{ allNullaryToStringTag = False, unwrapUnaryRecords = False } ''Simple01)
$(deriveBoth defaultOptions{ allNullaryToStringTag = False, unwrapUnaryRecords = True } ''Simple02)
$(deriveBoth defaultOptions{ allNullaryToStringTag = True, unwrapUnaryRecords = False } ''Simple03)
$(deriveBoth defaultOptions{ allNullaryToStringTag = True, unwrapUnaryRecords = True } ''Simple04)

$(deriveBoth defaultOptions{ allNullaryToStringTag = False, unwrapUnaryRecords = False, fieldLabelModifier = drop 4 } ''SimpleRecord01)
$(deriveBoth defaultOptions{ allNullaryToStringTag = False, unwrapUnaryRecords = True , fieldLabelModifier = drop 4 } ''SimpleRecord02)
$(deriveBoth defaultOptions{ allNullaryToStringTag = True , unwrapUnaryRecords = False, fieldLabelModifier = drop 4 } ''SimpleRecord03)
$(deriveBoth defaultOptions{ allNullaryToStringTag = True , unwrapUnaryRecords = True , fieldLabelModifier = drop 4 } ''SimpleRecord04)

$(deriveBoth defaultOptions{ sumEncoding = UntaggedValue } ''SumUntagged)

$(deriveBoth defaultOptions ''NT1)
$(deriveBoth defaultOptions { fieldLabelModifier = drop 4 } ''NT2)
$(deriveBoth defaultOptions { unwrapUnaryRecords = False }''NT3)
$(deriveBoth defaultOptions { fieldLabelModifier = drop 4, unwrapUnaryRecords = False } ''NT4)

instance Arbitrary a => Arbitrary (Record1 a) where
    arbitrary = Record1 <$> arbitrary <*> fmap Just arbitrary <*> arbitrary <*> fmap Just arbitrary
instance Arbitrary a => Arbitrary (Record2 a) where
    arbitrary = Record2 <$> arbitrary <*> fmap Just arbitrary <*> arbitrary <*> fmap Just arbitrary

arb :: Arbitrary a => (a -> b) -> (Maybe a -> b) -> (a -> a -> b) -> (a -> b) -> (Int -> Int -> b) -> Gen b
arb c1 c2 c3 c4 c5 = oneof
    [ c1 <$> arbitrary
    , c2 . Just <$> arbitrary
    , c3 <$> arbitrary <*> arbitrary
    , c4 <$> arbitrary
    , c5 <$> arbitrary <*> arbitrary
    ]

instance Arbitrary a => Arbitrary (Sum01 a) where arbitrary = arb Sum01A Sum01B Sum01C Sum01D Sum01E
instance Arbitrary a => Arbitrary (Sum02 a) where arbitrary = arb Sum02A Sum02B Sum02C Sum02D Sum02E
instance Arbitrary a => Arbitrary (Sum03 a) where arbitrary = arb Sum03A Sum03B Sum03C Sum03D Sum03E
instance Arbitrary a => Arbitrary (Sum04 a) where arbitrary = arb Sum04A Sum04B Sum04C Sum04D Sum04E
instance Arbitrary a => Arbitrary (Sum05 a) where arbitrary = arb Sum05A Sum05B Sum05C Sum05D Sum05E
instance Arbitrary a => Arbitrary (Sum06 a) where arbitrary = arb Sum06A Sum06B Sum06C Sum06D Sum06E
instance Arbitrary a => Arbitrary (Sum07 a) where arbitrary = arb Sum07A Sum07B Sum07C Sum07D Sum07E
instance Arbitrary a => Arbitrary (Sum08 a) where arbitrary = arb Sum08A Sum08B Sum08C Sum08D Sum08E
instance Arbitrary a => Arbitrary (Sum09 a) where arbitrary = arb Sum09A Sum09B Sum09C Sum09D Sum09E
instance Arbitrary a => Arbitrary (Sum10 a) where arbitrary = arb Sum10A Sum10B Sum10C Sum10D Sum10E
instance Arbitrary a => Arbitrary (Sum11 a) where arbitrary = arb Sum11A Sum11B Sum11C Sum11D Sum11E
instance Arbitrary a => Arbitrary (Sum12 a) where arbitrary = arb Sum12A Sum12B Sum12C Sum12D Sum12E

instance Arbitrary a => Arbitrary (Simple01 a) where arbitrary = Simple01 <$> arbitrary
instance Arbitrary a => Arbitrary (Simple02 a) where arbitrary = Simple02 <$> arbitrary
instance Arbitrary a => Arbitrary (Simple03 a) where arbitrary = Simple03 <$> arbitrary
instance Arbitrary a => Arbitrary (Simple04 a) where arbitrary = Simple04 <$> arbitrary
instance Arbitrary a => Arbitrary (SimpleRecord01 a) where arbitrary = SimpleRecord01 <$> arbitrary
instance Arbitrary a => Arbitrary (SimpleRecord02 a) where arbitrary = SimpleRecord02 <$> arbitrary
instance Arbitrary a => Arbitrary (SimpleRecord03 a) where arbitrary = SimpleRecord03 <$> arbitrary
instance Arbitrary a => Arbitrary (SimpleRecord04 a) where arbitrary = SimpleRecord04 <$> arbitrary

instance Arbitrary a => Arbitrary (SumUntagged a) where arbitrary = oneof [ SMInt <$> arbitrary, SMList <$> arbitrary ]

instance Arbitrary NT1 where arbitrary = fmap NT1 arbitrary
instance Arbitrary NT2 where arbitrary = fmap NT2 arbitrary
instance Arbitrary NT3 where arbitrary = fmap NT3 arbitrary
instance Arbitrary NT4 where arbitrary = fmap NT4 arbitrary

elmModuleContent :: String
elmModuleContent = unlines
    [ "module MyTests exposing (..)"
    , "-- This module requires the following packages:"
    , "-- * bartavelle/json-helpers"
    , "-- * NoRedInk/elm-json-decode-pipeline"
    , ""
    , "import Dict exposing (Dict)"
    , "import Expect exposing (Expectation, equal)"
    , "import Set exposing (Set)"
    , "import Json.Decode exposing (field, Value)"
    , "import Json.Encode"
    , "import Json.Helpers exposing (..)"
    , "import String"
    , "import Test exposing (Test, describe, test)"
    , ""
    , "newtypeDecode : Test"
    , "newtypeDecode = describe \"Newtype decoding checks\""
    , "              [ ntDecode1"
    , "              , ntDecode2"
    , "              , ntDecode3"
    , "              , ntDecode4"
    , "              ]"
    , ""
    , "newtypeEncode : Test"
    , "newtypeEncode = describe \"Newtype encoding checks\""
    , "              [ ntEncode1"
    , "              , ntEncode2"
    , "              , ntEncode3"
    , "              , ntEncode4"
    , "              ]"
    , ""
    , "recordDecode : Test"
    , "recordDecode = describe \"Record decoding checks\""
    , "              [ recordDecode1"
    , "              , recordDecode2"
    , "              ]"
    , ""
    , "recordEncode : Test"
    , "recordEncode = describe \"Record encoding checks\""
    , "              [ recordEncode1"
    , "              , recordEncode2"
    , "              ]"
    , ""
    , "sumDecode : Test"
    , "sumDecode = describe \"Sum decoding checks\""
    , "              [ sumDecode01"
    , "              , sumDecode02"
    , "              , sumDecode03"
    , "              , sumDecode04"
    , "              , sumDecode05"
    , "              , sumDecode06"
    , "              , sumDecode07"
    , "              , sumDecode08"
    , "              , sumDecode09"
    , "              , sumDecode10"
    , "              , sumDecode11"
    , "              , sumDecode12"
    , "              , sumDecodeUntagged"
    , "              ]"
    , ""
    , "sumEncode : Test"
    , "sumEncode = describe \"Sum encoding checks\""
    , "              [ sumEncode01"
    , "              , sumEncode02"
    , "              , sumEncode03"
    , "              , sumEncode04"
    , "              , sumEncode05"
    , "              , sumEncode06"
    , "              , sumEncode07"
    , "              , sumEncode08"
    , "              , sumEncode09"
    , "              , sumEncode10"
    , "              , sumEncode11"
    , "              , sumEncode12"
    , "              , sumEncodeUntagged"
    , "              ]"
    , ""
    , "simpleDecode : Test"
    , "simpleDecode = describe \"Simple records/types decode checks\""
    , "                [ simpleDecode01"
    , "                , simpleDecode02"
    , "                , simpleDecode03"
    , "                , simpleDecode04"
    , "                , simplerecordDecode01"
    , "                , simplerecordDecode02"
    , "                , simplerecordDecode03"
    , "                , simplerecordDecode04"
    , "                ]"
    , ""
    , "simpleEncode : Test"
    , "simpleEncode = describe \"Simple records/types encode checks\""
    , "                [ simpleEncode01"
    , "                , simpleEncode02"
    , "                , simpleEncode03"
    , "                , simpleEncode04"
    , "                , simplerecordEncode01"
    , "                , simplerecordEncode02"
    , "                , simplerecordEncode03"
    , "                , simplerecordEncode04"
    , "                ]"
    , ""
    , "-- this is done to prevent artificial differences due to object ordering, this won't work with Maybe's though :("
    , "equalHack : String -> String -> Expectation"
    , "equalHack a b ="
    , "    let remix = Json.Decode.decodeString Json.Decode.value"
    , "    in equal (remix a) (remix b)"
    , ""
    , ""
    , makeModuleContentWithAlterations (newtypeAliases ["Record1", "Record2", "SimpleRecord01", "SimpleRecord02", "SimpleRecord03", "SimpleRecord04"] . defaultAlterations)
        [ DefineElm (Proxy :: Proxy (Record1 a))
        , DefineElm (Proxy :: Proxy (Record2 a))
        , DefineElm (Proxy :: Proxy (Sum01 a))
        , DefineElm (Proxy :: Proxy (Sum02 a))
        , DefineElm (Proxy :: Proxy (Sum03 a))
        , DefineElm (Proxy :: Proxy (Sum04 a))
        , DefineElm (Proxy :: Proxy (Sum05 a))
        , DefineElm (Proxy :: Proxy (Sum06 a))
        , DefineElm (Proxy :: Proxy (Sum07 a))
        , DefineElm (Proxy :: Proxy (Sum08 a))
        , DefineElm (Proxy :: Proxy (Sum09 a))
        , DefineElm (Proxy :: Proxy (Sum10 a))
        , DefineElm (Proxy :: Proxy (Sum11 a))
        , DefineElm (Proxy :: Proxy (Sum12 a))
        , DefineElm (Proxy :: Proxy (Simple01 a))
        , DefineElm (Proxy :: Proxy (Simple02 a))
        , DefineElm (Proxy :: Proxy (Simple03 a))
        , DefineElm (Proxy :: Proxy (Simple04 a))
        , DefineElm (Proxy :: Proxy (SimpleRecord01 a))
        , DefineElm (Proxy :: Proxy (SimpleRecord02 a))
        , DefineElm (Proxy :: Proxy (SimpleRecord03 a))
        , DefineElm (Proxy :: Proxy (SimpleRecord04 a))
        , DefineElm (Proxy :: Proxy (SumUntagged a))
        , DefineElm (Proxy :: Proxy NT1)
        , DefineElm (Proxy :: Proxy NT2)
        , DefineElm (Proxy :: Proxy NT3)
        , DefineElm (Proxy :: Proxy NT4)
        ]
    ]


main :: IO ()
main = do
    ss01 <- sample' arbitrary :: IO [Sum01 [Int]]
    ss02 <- sample' arbitrary :: IO [Sum02 [Int]]
    ss03 <- sample' arbitrary :: IO [Sum03 [Int]]
    ss04 <- sample' arbitrary :: IO [Sum04 [Int]]
    ss05 <- sample' arbitrary :: IO [Sum05 [Int]]
    ss06 <- sample' arbitrary :: IO [Sum06 [Int]]
    ss07 <- sample' arbitrary :: IO [Sum07 [Int]]
    ss08 <- sample' arbitrary :: IO [Sum08 [Int]]
    ss09 <- sample' arbitrary :: IO [Sum09 [Int]]
    ss10 <- sample' arbitrary :: IO [Sum10 [Int]]
    ss11 <- sample' arbitrary :: IO [Sum11 [Int]]
    ss12 <- sample' arbitrary :: IO [Sum12 [Int]]
    re01 <- sample' arbitrary :: IO [Record1 [Int]]
    re02 <- sample' arbitrary :: IO [Record2 [Int]]
    sp01 <- sample' arbitrary :: IO [Simple01 [Int]]
    sp02 <- sample' arbitrary :: IO [Simple02 [Int]]
    sp03 <- sample' arbitrary :: IO [Simple03 [Int]]
    sp04 <- sample' arbitrary :: IO [Simple04 [Int]]
    sr01 <- sample' arbitrary :: IO [SimpleRecord01 [Int]]
    sr02 <- sample' arbitrary :: IO [SimpleRecord02 [Int]]
    sr03 <- sample' arbitrary :: IO [SimpleRecord03 [Int]]
    sr04 <- sample' arbitrary :: IO [SimpleRecord04 [Int]]
    sm   <- sample' arbitrary :: IO [SumUntagged [Int]]
    nt1  <- sample' arbitrary :: IO [NT1]
    nt2  <- sample' arbitrary :: IO [NT2]
    nt3  <- sample' arbitrary :: IO [NT3]
    nt4  <- sample' arbitrary :: IO [NT4]
    args <- getArgs
    case args of
        [] -> return ()
        (x:_) -> writeFile x $
               unlines [ elmModuleContent
                       , mkSumEncodeTest "01" ss01
                       , mkSumEncodeTest "02" ss02
                       , mkSumEncodeTest "03" ss03
                       , mkSumEncodeTest "04" ss04
                       , mkSumEncodeTest "05" ss05
                       , mkSumEncodeTest "06" ss06
                       , mkSumEncodeTest "07" ss07
                       , mkSumEncodeTest "08" ss08
                       , mkSumEncodeTest "09" ss09
                       , mkSumEncodeTest "10" ss10
                       , mkSumEncodeTest "11" ss11
                       , mkSumEncodeTest "12" ss12
                       , mkSumDecodeTest "01" ss01
                       , mkSumDecodeTest "02" ss02
                       , mkSumDecodeTest "03" ss03
                       , mkSumDecodeTest "04" ss04
                       , mkSumDecodeTest "05" ss05
                       , mkSumDecodeTest "06" ss06
                       , mkSumDecodeTest "07" ss07
                       , mkSumDecodeTest "08" ss08
                       , mkSumDecodeTest "09" ss09
                       , mkSumDecodeTest "10" ss10
                       , mkSumDecodeTest "11" ss11
                       , mkSumDecodeTest "12" ss12
                       , mkRecordDecodeTest "1" re01
                       , mkRecordDecodeTest "2" re02
                       , mkRecordEncodeTest "1" re01
                       , mkRecordEncodeTest "2" re02
                       , mkSimpleEncodeTest "01" sp01
                       , mkSimpleEncodeTest "02" sp02
                       , mkSimpleEncodeTest "03" sp03
                       , mkSimpleEncodeTest "04" sp04
                       , mkSimpleDecodeTest "01" sp01
                       , mkSimpleDecodeTest "02" sp02
                       , mkSimpleDecodeTest "03" sp03
                       , mkSimpleDecodeTest "04" sp04
                       , mkSimpleRecordEncodeTest "01" sr01
                       , mkSimpleRecordEncodeTest "02" sr02
                       , mkSimpleRecordEncodeTest "03" sr03
                       , mkSimpleRecordEncodeTest "04" sr04
                       , mkSimpleRecordDecodeTest "01" sr01
                       , mkSimpleRecordDecodeTest "02" sr02
                       , mkSimpleRecordDecodeTest "03" sr03
                       , mkSimpleRecordDecodeTest "04" sr04
                       , mkSumEncodeTest "Untagged" sm
                       , mkSumDecodeTest "Untagged" sm
                       , mkDecodeTestNT "NT" "_nt" "1" extractNT1 nt1
                       , mkEncodeTestNT "NT" "_nt" "1" extractNT1 nt1
                       , mkDecodeTestNT "NT" "_nt" "2" extractNT2 nt2
                       , mkEncodeTestNT "NT" "_nt" "2" extractNT2 nt2
                       , mkDecodeTestNT "NT" "_nt" "3" extractNT3 nt3
                       , mkEncodeTestNT "NT" "_nt" "3" extractNT3 nt3
                       , dropAll "(Json.Decode.list Json.Decode.int)" (mkDecodeTest "NT" "_nt" "4" nt4)
                       , dropAll "(Json.Encode.list Json.Encode.int)" (mkEncodeTest "NT" "_nt" "4" nt4)
                       ]

