module Elm.TyRepSpec (spec) where

import Elm.TyRep

import Data.Proxy
import Test.Hspec

spec :: Spec
spec =
    describe "toElmType" $
      it "should produce the correct code" $
      do toElmType (Proxy :: Proxy Int) `shouldBe` ETyCon (ETCon "Int")
         toElmType (Proxy :: Proxy Float) `shouldBe` ETyCon (ETCon "Float")
         toElmType (Proxy :: Proxy String) `shouldBe` ETyCon (ETCon "String")
         toElmType (Proxy :: Proxy Bool) `shouldBe` ETyCon (ETCon "Bool")
         toElmType (Proxy :: Proxy Char) `shouldBe` ETyCon (ETCon "Char")
         toElmType (Proxy :: Proxy [Int]) `shouldBe` ETyApp (ETyCon $ ETCon "List") (ETyCon $ ETCon "Int")
         toElmType (Proxy :: Proxy (Maybe Int)) `shouldBe` ETyApp (ETyCon $ ETCon "Maybe") (ETyCon $ ETCon "Int")
         toElmType (Proxy :: Proxy ()) `shouldBe` ETyTuple 0
         toElmType (Proxy :: Proxy (Int, Bool)) `shouldBe` ETyApp (ETyApp (ETyTuple 2) (ETyCon $ ETCon "Int")) (ETyCon $ ETCon "Bool")
         toElmType (Proxy :: Proxy (Int, Bool, String)) `shouldBe` ETyApp (ETyApp (ETyApp (ETyTuple 3) (ETyCon $ ETCon "Int")) (ETyCon $ ETCon "Bool")) (ETyCon $ ETCon "String")
