{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Elm.Module where

import Data.Proxy
import Data.List

import Elm.TyRep
import Elm.TyRender
import Elm.Json

data DefineElm
   = forall a. IsElmDefinition a => DefineElm (Proxy a)

makeElmModule :: String -> [DefineElm] -> String
makeElmModule moduleName defs =
    "module " ++ moduleName ++ " where \n"
    ++ intercalate "\n\n" (map mkDef defs)
    where
      mkDef (DefineElm proxy) =
          let def = compileElmDef proxy
          in renderElm def ++ "\n" ++ jsonParserForDef def ++ "\n" ++ jsonSerForDef def ++ "\n"
