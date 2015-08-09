{-# LANGUAGE TemplateHaskell #-}
module Elm.DeriveSpec (spec) where

import Elm.Derive
import Elm.TyRep

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

deriveElmDef defaultOpts ''Foo
deriveElmDef defaultOpts ''Bar
deriveElmDef defaultOpts ''SomeOpts

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
        [ ("Okay",[ETyCon (ETCon {tc_name = "Int"})])
        , ("NotOkay",[ETyVar (ETVar {tv_name = "a"})])
        ]
    }

spec :: Spec
spec =
    describe "deriveElmRep" $
    it "should produce the correct types" $
    do compileElmDef (Proxy :: Proxy Foo) `shouldBe` fooElm
       compileElmDef (Proxy :: Proxy (Bar a)) `shouldBe` barElm
       compileElmDef (Proxy :: Proxy (SomeOpts a)) `shouldBe` someOptsElm
