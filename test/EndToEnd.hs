{-# LANGUAGE TemplateHaskell #-}
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

instance Arbitrary a => Arbitrary (Record1 a) where
    arbitrary = Record1 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary a => Arbitrary (Record2 a) where
    arbitrary = Record2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

arb :: Arbitrary a => (a -> b) -> (Maybe a -> b) -> (a -> a -> b) -> (a -> b) -> (Int -> Int -> b) -> Gen b
arb c1 c2 c3 c4 c5 = oneof
    [ c1 <$> arbitrary
    , c2 <$> arbitrary
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

elmModuleContent :: String
elmModuleContent = unlines
    [ "module MyTests where"
    , ""
    , "import Dict exposing(Dict)"
    , "import Json.Decode exposing ((:=), Value)"
    , "import Json.Encode"
    , "import Json.Bridge.Helpers exposing (..)"
    , "import ElmTest exposing (..)"
    , "import Graphics.Element exposing (Element)"
    , "import String"
    , ""
    , "main : Element"
    , "main = elementRunner <| suite \"Testing\" [ sumEncode, sumDecode ]"
    ,""
    , "sumDecode : Test"
    , "sumDecode = suite \"Sum decoding checks\""
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
    , "              ]"
    , ""
    , "sumEncode : Test"
    , "sumEncode = suite \"Sum encoding checks\""
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
    , "              ]"
    , ""
    , "-- this is done to prevent artificial differences due to object ordering, this is hacky as hell :("
    , "assertEqualHack : String -> String -> Assertion"
    , "assertEqualHack a b ="
    , "    let remix = String.map replaceSpecial >> String.split \",\" >> List.sort"
    , "        replaceSpecial c = case c of"
    , "            '{' -> ','"
    , "            '}' -> ','"
    , "            '[' -> ','"
    , "            ']' -> ','"
    , "            x   -> x"
    , "    in assertEqual (remix a) (remix b)"
    , ""
    , makeModuleContent
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
        ]
    ]

mkSumDecodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSumDecodeTest num elems = unlines (
    [ "sumDecode" ++ num ++ " : Test"
    , "sumDecode" ++ num ++ " = suite \"sum decode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = prefix ++ "test \"" ++ show n ++ "\" (assertEqual (Json.Decode.decodeString (jsonDecSum" ++ num ++ " Json.Decode.int) " ++ encoded ++ ") (Ok (" ++ pretty ++ ")))"
        where
            pretty = T.unpack $ T.replace (T.pack ("_s" ++ num)) T.empty $ T.pack $ show e
            encoded = show (encode e)
            prefix = if n == 1 then "  [ " else "  , "

mkSumEncodeTest :: (Show a, ToJSON a) => String -> [a] -> String
mkSumEncodeTest num elems = unlines (
    [ "sumEncode" ++ num ++ " : Test"
    , "sumEncode" ++ num ++ " = suite \"sum encode " ++ num ++ "\""
    ]
    ++ map mktest (zip ([1..] :: [Int]) elems)
    ++ ["  ]"]
    )
  where
      mktest (n,e) = prefix ++ "test \"" ++ show n ++ "\" (assertEqualHack (Json.Encode.encode 0 (jsonEncSum" ++ num ++ " Json.Encode.int (" ++ pretty ++ "))) " ++ encoded ++ ")"
        where
            pretty = T.unpack $ T.replace (T.pack ("_s" ++ num)) T.empty $ T.pack $ show e
            encoded = show (encode e)
            prefix = if n == 1 then "  [ " else "  , "

main :: IO ()
main = do
    ss01 <- sample' arbitrary :: IO [Sum01 Int]
    ss02 <- sample' arbitrary :: IO [Sum02 Int]
    ss03 <- sample' arbitrary :: IO [Sum03 Int]
    ss04 <- sample' arbitrary :: IO [Sum04 Int]
    ss05 <- sample' arbitrary :: IO [Sum05 Int]
    ss06 <- sample' arbitrary :: IO [Sum06 Int]
    ss07 <- sample' arbitrary :: IO [Sum07 Int]
    ss08 <- sample' arbitrary :: IO [Sum08 Int]
    ss09 <- sample' arbitrary :: IO [Sum09 Int]
    ss10 <- sample' arbitrary :: IO [Sum10 Int]
    ss11 <- sample' arbitrary :: IO [Sum11 Int]
    ss12 <- sample' arbitrary :: IO [Sum12 Int]
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
                       ]

