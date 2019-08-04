module Main where

import qualified Elm.DeriveSpec
import qualified Elm.TyRenderSpec
import qualified Elm.JsonSpec
import qualified Elm.ModuleSpec
import qualified Elm.TyRepSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Elm.DeriveSpec" Elm.DeriveSpec.spec
  describe "Elm.TyRenderSpec" Elm.TyRenderSpec.spec
  describe "Elm.JsonSpec" Elm.JsonSpec.spec
  describe "Elm.ModuleSpec" Elm.ModuleSpec.spec
  describe "Elm.TyRepSpec" Elm.TyRepSpec.spec
