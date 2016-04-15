{-# LANGUAGE TemplateHaskell #-}
module Elm.DeriveSpec (spec) where

import Elm.Derive
import Elm.TyRep

import Data.Proxy
import Test.Hspec
import Data.Char (toLower)

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

data Change a = Change { _before :: a, _after :: a }

data Baz a = Baz1 { _fOo :: Int, _qux :: a }
           | Baz2 { _bar :: Int, _sTr :: String }
           | Zob a

data Qux a = Qux1 { _quxfoo :: Int, _quxqux :: a }
           | Qux2 { _quxbar :: Int, _quxstr :: String }

data Test a = Test { _t1 :: Change Int
                   , _t2 :: Change a
                   }

data SomeOpts a
   = Okay Int
   | NotOkay a

deriveElmDef defaultOptions ''Foo
deriveElmDef defaultOptions ''Bar
deriveElmDef defaultOptions ''SomeOpts
deriveElmDef defaultOptions { fieldLabelModifier = drop 1 . map toLower } ''Baz
deriveElmDef defaultOptions { fieldLabelModifier = drop 1 . map toLower } ''Test
deriveElmDef defaultOptions { fieldLabelModifier = drop 4 . map toLower, sumEncoding = TaggedObject "key" "value" } ''Qux

testElm :: ETypeDef
testElm = ETypeAlias $ EAlias
    { ea_name =
        ETypeName
        { et_name = "Test"
        , et_args = [ETVar {tv_name = "a"}]
        }
    , ea_fields =
        [ ("t1",ETyApp (ETyCon (ETCon {tc_name = "Change"})) (ETyCon (ETCon {tc_name = "Int"})))
        , ("t2",ETyApp (ETyCon (ETCon {tc_name = "Change"})) (ETyVar (ETVar {tv_name = "a"})))
        ]
    , ea_omit_null = False
    , ea_newtype = False
    , ea_unwrap_unary = False
    }

fooElm :: ETypeDef
fooElm =
    ETypeAlias $
    EAlias
    { ea_name =
          ETypeName
          { et_name = "Foo"
          , et_args = []
          }
    , ea_fields =
        [("f_name",ETyCon (ETCon {tc_name = "String"})),("f_blablub",ETyCon (ETCon {tc_name = "Int"}))]
    , ea_omit_null = False
    , ea_newtype = False
    , ea_unwrap_unary = False
    }

barElm :: ETypeDef
barElm =
    ETypeAlias $
    EAlias
    { ea_name =
          ETypeName
          { et_name = "Bar"
          , et_args = [ETVar {tv_name = "a"}]
          }
    , ea_fields =
        [ ("b_name",ETyVar (ETVar {tv_name = "a"}))
        , ("b_blablub",ETyCon (ETCon {tc_name = "Int"}))
        , ("b_tuple",ETyApp (ETyApp (ETyTuple 2) (ETyCon (ETCon {tc_name = "Int"}))) (ETyCon (ETCon {tc_name = "String"})))
        , ("b_list",ETyApp (ETyCon (ETCon {tc_name = "List"})) (ETyCon (ETCon {tc_name = "Bool"})))
        ]
    , ea_omit_null = False
    , ea_newtype = False
    , ea_unwrap_unary = False
    }

bazElm :: ETypeDef
bazElm = ETypeSum $ ESum
    { es_name = ETypeName {et_name = "Baz", et_args = [ETVar {tv_name = "a"}]}
    , es_options =
        [ ("Baz1",Left [("foo",ETyCon (ETCon {tc_name = "Int"})), ("qux",ETyVar (ETVar {tv_name = "a"}))])
        , ("Baz2",Left [("bar",ETyCon (ETCon {tc_name = "Int"})), ("str",ETyCon (ETCon {tc_name = "String"}))])
        , ("Zob",Right [ETyVar (ETVar {tv_name = "a"})])
        ]
    , es_type = SumEncoding' ObjectWithSingleField
    , es_omit_null = False
    , es_unary_strings = True
    }

quxElm :: ETypeDef
quxElm = ETypeSum $ ESum
    { es_name = ETypeName {et_name = "Qux", et_args = [ETVar {tv_name = "a"}]}
    , es_options =
        [ ("Qux1",Left [("foo",ETyCon (ETCon {tc_name = "Int"})), ("qux",ETyVar (ETVar {tv_name = "a"}))])
        , ("Qux2",Left [("bar",ETyCon (ETCon {tc_name = "Int"})), ("str",ETyCon (ETCon {tc_name = "String"}))])
        ]
    , es_type = SumEncoding' $ TaggedObject "key" "value"
    , es_omit_null = False
    , es_unary_strings = True
    }

someOptsElm :: ETypeDef
someOptsElm =
    ETypeSum $
    ESum
    { es_name =
          ETypeName
          { et_name = "SomeOpts"
          , et_args = [ETVar {tv_name = "a"}]
          }
    , es_options =
        [ ("Okay", Right [ETyCon (ETCon {tc_name = "Int"})])
        , ("NotOkay", Right [ETyVar (ETVar {tv_name = "a"})])
        ]
    , es_type = defSumEncoding
    , es_omit_null = False
    , es_unary_strings = True
    }

spec :: Spec
spec =
    describe "deriveElmRep" $
    it "should produce the correct types" $
    do compileElmDef (Proxy :: Proxy Foo) `shouldBe` fooElm
       compileElmDef (Proxy :: Proxy (Bar a)) `shouldBe` barElm
       compileElmDef (Proxy :: Proxy (SomeOpts a)) `shouldBe` someOptsElm
       compileElmDef (Proxy :: Proxy (Baz a)) `shouldBe` bazElm
       compileElmDef (Proxy :: Proxy (Qux a)) `shouldBe` quxElm
       compileElmDef (Proxy :: Proxy (Test a)) `shouldBe` testElm
