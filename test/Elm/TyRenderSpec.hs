{-# LANGUAGE TemplateHaskell #-}
module Elm.TyRenderSpec (spec) where

import Elm.Derive
import Elm.TyRep
import Elm.TyRender

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

data Unit
   = Unit
   { u_unit :: ()
   }

data Paa
    = PA1
    | PA2

newtype PhantomA a = PhantomA Int
newtype PhantomB a = PhantomB { getPhantomB :: Int }
newtype PhantomC a = PhantomC Int
newtype PhantomD a = PhantomD { getPhantomD :: Int }

$(deriveElmDef (defaultOptionsDropLower 2) ''Foo)
$(deriveElmDef (defaultOptionsDropLower 2) ''Bar)
$(deriveElmDef defaultOptions ''SomeOpts)
$(deriveElmDef defaultOptions ''Unit)
$(deriveElmDef defaultOptions{allNullaryToStringTag = True, constructorTagModifier = drop 1} ''Paa)
$(deriveElmDef defaultOptions ''PhantomA)
$(deriveElmDef defaultOptions ''PhantomB)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''PhantomC)
$(deriveElmDef defaultOptions { unwrapUnaryRecords = False } ''PhantomD)

fooCode :: String
fooCode = "type alias Foo  =\n   { name: String\n   , blablub: Int\n   }\n"

barCode :: String
barCode = "type alias Bar a =\n   { name: a\n   , blablub: Int\n   , tuple: (Int, String)\n   , list: (List Bool)\n   }\n"

someOptsCode :: String
someOptsCode = "type SomeOpts a =\n    Okay Int\n    | NotOkay a\n"

unitCode :: String
unitCode = "type alias Unit  =\n   { u_unit: ()\n   }\n"

paaCode :: String
paaCode = unlines
  [ "type Paa  ="
  , "    PA1 "
  , "    | PA2 "
  ]

phantomATy :: String
phantomATy = "type alias PhantomA a = Int\n"
phantomBTy :: String
phantomBTy = "type alias PhantomB a = Int\n"
phantomCTy :: String
phantomCTy = "type alias PhantomC a = Int\n"
phantomDTy :: String
phantomDTy = "type PhantomD a = PhantomD\n   { getPhantomD: Int\n   }\n"

spec :: Spec
spec =
    describe "deriveElmRep" $
    do let rFoo = compileElmDef (Proxy :: Proxy Foo)
           rBar = compileElmDef (Proxy :: Proxy (Bar a))
           rSomeOpts = compileElmDef (Proxy :: Proxy (SomeOpts a))
           rUnit = compileElmDef (Proxy :: Proxy Unit)
           rPaa = compileElmDef (Proxy :: Proxy Paa)
           rPhA = compileElmDef (Proxy :: Proxy (PhantomA a))
           rPhB = compileElmDef (Proxy :: Proxy (PhantomB a))
           rPhC = compileElmDef (Proxy :: Proxy (PhantomC a))
           rPhD = compileElmDef (Proxy :: Proxy (PhantomD a))
       it "should produce the correct code" $
          do renderElm rFoo `shouldBe` fooCode
             renderElm rBar `shouldBe` barCode
             renderElm rSomeOpts `shouldBe` someOptsCode
             renderElm rUnit `shouldBe` unitCode
             renderElm rPaa `shouldBe` paaCode
             renderElm rPhA `shouldBe` phantomATy
             renderElm rPhB `shouldBe` phantomBTy
             renderElm rPhC `shouldBe` phantomCTy
             renderElm rPhD `shouldBe` phantomDTy
