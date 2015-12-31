{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Elm.Module where

import Data.Proxy
import Data.List
import Control.Arrow (second, (+++))

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
makeModuleContent = makeModuleContentWithAlterations id

makeModuleContentWithAlterations :: (ETypeDef -> ETypeDef) -> [DefineElm] -> String
makeModuleContentWithAlterations alt = intercalate "\n\n" . map mkDef
    where
      mkDef (DefineElm proxy) =
          let def = alt (compileElmDef proxy)
          in renderElm def ++ "\n" ++ jsonParserForDef def ++ "\n" ++ jsonSerForDef def ++ "\n"

recAlterType :: (EType -> EType) -> ETypeDef -> ETypeDef
recAlterType f td = case td of
                     ETypeAlias a -> ETypeAlias (a { ea_fields = map (second f') (ea_fields a) })
                     ETypePrimAlias (EPrimAlias n t) -> ETypePrimAlias (EPrimAlias n (f' t))
                     ETypeSum s -> ETypeSum (s { es_options = map (second (map (second f') +++ map f')) (es_options s) })
    where
        f' (ETyApp a b) = f (ETyApp (f' a) (f' b))
        f' x = f x

