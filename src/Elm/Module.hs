{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-|
Functions in this module are used to generate Elm modules. Note that the generated modules depend on the @bartavelle/json-helpers@ package.

-}
module Elm.Module where

import           Control.Arrow (second, (+++))
import           Data.List
import           Data.Proxy

import           Elm.Json
import           Elm.TyRender
import           Elm.TyRep
import           Elm.Versions

-- | Existential quantification wrapper for lists of type definitions
data DefineElm
   = forall a. IsElmDefinition a => DefineElm (Proxy a)

-- | The module header line for this version of Elm
moduleHeader :: ElmVersion
             -> String
             -> String
moduleHeader Elm0p18 moduleName = "module " ++ moduleName ++ " exposing(..)"

-- | Creates an Elm module for the given version. This will use the default
-- type conversion rules (to -- convert @Vector@ to @List@, @HashMap a b@
-- to @List (a,b)@, etc.).
makeElmModuleWithVersion :: ElmVersion
                         -> String  -- ^ Module name
                         -> [DefineElm]  -- ^ List of definitions to be included in the module
                         -> String
makeElmModuleWithVersion elmVersion moduleName defs = unlines
    [ moduleHeader elmVersion moduleName
    , ""
    , "import Json.Decode"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import EveryDict"
    , "import Set"
    , ""
    , ""
    ] ++ makeModuleContent defs

-- | Creates an Elm module. This will use the default type conversion rules (to
-- convert @Vector@ to @List@, @HashMap a b@ to @List (a,b)@, etc.).
makeElmModule :: String -- ^ Module name
              -> [DefineElm] -- ^ List of definitions to be included in the module
              -> String
makeElmModule = makeElmModuleWithVersion Elm0p18

-- | Generates the content of a module. You will be responsible for
-- including the required Elm headers. This uses the default type
-- conversion rules.
makeModuleContent :: [DefineElm] -> String
makeModuleContent = makeModuleContentWithAlterations defaultAlterations

-- | Generates the content of a module, using custom type conversion rules.
makeModuleContentWithAlterations :: (ETypeDef -> ETypeDef) -> [DefineElm] -> String
makeModuleContentWithAlterations alt = intercalate "\n\n" . map mkDef
    where
      mkDef (DefineElm proxy) =
          let def = alt (compileElmDef proxy)
          in renderElm def ++ "\n" ++ jsonParserForDef def ++ "\n" ++ jsonSerForDef def ++ "\n"

{-| A helper function that will recursively traverse type definitions and let you convert types.

> myAlteration : ETypeDef -> ETypeDef
> myAlteration = recAlterType $ \t -> case t of
>                   ETyCon (ETCon "Integer") -> ETyCon (ETCon "Int")
>                   ETyCon (ETCon "Text")    -> ETyCon (ETCon "String")
>                   _                        -> t

-}
recAlterType :: (EType -> EType) -> ETypeDef -> ETypeDef
recAlterType f td = case td of
                     ETypeAlias a -> ETypeAlias (a { ea_fields = map (second f') (ea_fields a) })
                     ETypePrimAlias (EPrimAlias n t) -> ETypePrimAlias (EPrimAlias n (f' t))
                     ETypeSum s -> ETypeSum (s { es_options = map (second (map (second f') +++ map f')) (es_options s) })
    where
        f' (ETyApp a b) = f (ETyApp (f' a) (f' b))
        f' x            = f x

-- | Given a list of type names, will @newtype@ all the matching type
-- definitions.
newtypeAliases :: [String] -> ETypeDef -> ETypeDef
newtypeAliases nts (ETypeAlias e) = ETypeAlias $ if et_name (ea_name e) `elem` nts
                                                     then e { ea_newtype = True }
                                                     else e
newtypeAliases _ x = x

{-| A default set of type conversion rules:

 * @HashSet a@, @Set a@ -> if @a@ is comparable, then @Set a@, else @List a@
 * @HashMap String v@, @Map String v@ -> @Dict String v@
 * @HashMap k v@, @Map k v@ -> @List (k, v)@
 * @Integer@ -> @Int@
 * @Text@ -> @String@
 * @Vector@ -> @List@
 * @Double@ -> @Float@
-}
defaultAlterations :: ETypeDef -> ETypeDef
defaultAlterations = recAlterType $ \t -> case t of
                                  ETyApp (ETyCon (ETCon "HashSet")) s             -> checkSet s
                                  ETyApp (ETyCon (ETCon "Set")) s                 -> checkSet s
                                  ETyApp (ETyApp (ETyCon (ETCon "HashMap")) k) v  -> ETyApp (ETyApp (tc "EveryDict") k) v
                                  ETyApp (ETyApp (ETyCon (ETCon "THashMap")) k) v -> ETyApp (ETyApp (tc "EveryDict") k) v
                                  ETyApp (ETyApp (ETyCon (ETCon "Map")) k) v      -> ETyApp (ETyApp (tc "EveryDict") k) v
                                  ETyCon (ETCon "Integer")                        -> ETyCon (ETCon "Int")
                                  ETyCon (ETCon "Text")                           -> ETyCon (ETCon "String")
                                  ETyCon (ETCon "Vector")                         -> ETyCon (ETCon "List")
                                  ETyCon (ETCon "Double")                         -> ETyCon (ETCon "Float")
                                  _                                               -> t
    where
        isComparable (ETyCon (ETCon n)) = n `elem` ["String", "Int"]
        isComparable _                  = False -- TODO check what Elm actually uses
        tc = ETyCon . ETCon
        checkSet s | isComparable s = ETyApp (ETyCon (ETCon "Set")) s
                   | otherwise = ETyApp (ETyCon (ETCon "List")) s
