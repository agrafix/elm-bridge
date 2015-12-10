{-# LANGUAGE TemplateHaskell #-}
module Elm.TyRenderSpec (spec) where

import Elm.Derive
import Elm.TyRep
import Elm.TyRender

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
$(deriveElmDef defaultOptions ''SomeOpts)

fooCode :: String
fooCode = "type alias Foo  = \n   { name: String\n   , blablub: Int\n   }\n"

barCode :: String
barCode = "type alias Bar a = \n   { name: a\n   , blablub: Int\n   , tuple: (Int, String)\n   , list: (List Bool)\n   }\n"

someOptsCode :: String
someOptsCode = "type SomeOpts a = \n    Okay Int\n    | NotOkay a\n"

spec :: Spec
spec =
    describe "deriveElmRep" $
    do let rFoo = compileElmDef (Proxy :: Proxy Foo)
           rBar = compileElmDef (Proxy :: Proxy (Bar a))
           rSomeOpts = compileElmDef (Proxy :: Proxy (SomeOpts a))
       it "should produce the correct code" $
          do renderElm rFoo `shouldBe` fooCode
             renderElm rBar `shouldBe` barCode
             renderElm rSomeOpts `shouldBe` someOptsCode
