{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Elm.Module where

import Data.Proxy
import Data.List

import Elm.TyRep
import Elm.TyRender
import Elm.Json

-- | Existential quantification wrapper for lists of type definitions
data DefineElm
   = forall a. IsElmDefinition a => DefineElm (Proxy a)

-- | Compile an Elm module
makeElmModule :: String -> [DefineElm] -> String
makeElmModule moduleName defs = unlines (
    [ "module " ++ moduleName ++ " where"
    , ""
    , "import Json.Decode"
    , "import Json.Decode exposing ((:=))"
    , "import Json.Encode"
    , ""
    , ""
    ]) ++ makeModuleContent defs

makeModuleContent :: [DefineElm] -> String
makeModuleContent = intercalate "\n\n" . map mkDef
    where
      mkDef (DefineElm proxy) =
          let def = compileElmDef proxy
          in renderElm def ++ "\n" ++ jsonParserForDef def ++ "\n" ++ jsonSerForDef def ++ "\n"
