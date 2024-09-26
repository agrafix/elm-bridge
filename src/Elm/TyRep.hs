{-| This module defines how the derived Haskell data types are represented.
- It is useful for writing type conversion rules.
-}
module Elm.TyRep where

import qualified Data.Char        as Char
import           Data.List
import           Data.Proxy
import           Data.Typeable    (TyCon, TypeRep, Typeable, splitTyConApp,
                                   tyConName, typeRep, typeRepTyCon)

import           Data.Aeson.Types (SumEncoding (..))
import           Data.Maybe       (fromMaybe)

-- | Type definition, including constructors.
data ETypeDef
   = ETypeAlias EAlias
   | ETypePrimAlias EPrimAlias
   | ETypeSum ESum
     deriving (Show, Eq)

-- | Type construction : type variables, type constructors, tuples and type
-- application.
data EType
   = ETyVar ETVar
   | ETyCon ETCon
   | ETyApp EType EType
   | ETyTuple Int
   deriving (Show, Eq, Ord)

{-| Type constructor:

> ETCon "Int"
-}
newtype ETCon
   = ETCon
   { tc_name :: String
   } deriving (Show, Eq, Ord)

{-| Type variable:

> ETVar "a"
-}
newtype ETVar
   = ETVar
   { tv_name :: String
   } deriving (Show, Eq, Ord)


{-| Type name:

> ETypeName "Map" [ETVar "k", ETVar "v"]
-}
data ETypeName
   = ETypeName
   { et_name :: String
   , et_args :: [ETVar]
   } deriving (Show, Eq, Ord)

data EPrimAlias
   = EPrimAlias
   { epa_name :: ETypeName
   , epa_type :: EType
   } deriving (Show, Eq, Ord)

data EAlias
   = EAlias
   { ea_name         :: ETypeName
   , ea_fields       :: [(String, EType)]
   , ea_omit_null    :: Bool
   , ea_newtype      :: Bool
   , ea_unwrap_unary :: Bool
   } deriving (Show, Eq, Ord)

data SumTypeFields
    = Anonymous [EType]
    | Named [(String, EType)]
    deriving (Show, Eq, Ord)

isNamed :: SumTypeFields -> Bool
isNamed s =
    case s of
      Named _ -> True
      _       -> False

isEmpty :: SumTypeFields -> Bool
isEmpty (Anonymous []) = True
isEmpty (Named [])     = True
isEmpty _              = False

data SumTypeConstructor
    = STC
    { _stcName    :: String
    , _stcEncoded :: String
    , _stcFields  :: SumTypeFields
    } deriving (Show, Eq, Ord)

data ESum
    = ESum
    { es_name          :: ETypeName
    , es_constructors  :: [SumTypeConstructor]
    , es_type          :: SumEncoding'
    , es_omit_null     :: Bool
    , es_unary_strings :: Bool
    } deriving (Show, Eq, Ord)

-- | Transforms tuple types in a list of types. Otherwise returns
-- a singleton list with the original type.
unpackTupleType :: EType -> [EType]
unpackTupleType et = fromMaybe [et] (extract et)
    where
        extract :: EType -> Maybe [EType]
        extract ty = case ty of
                         ETyTuple 0 -> return []
                         ETyApp (ETyTuple _) t -> return [t]
                         ETyApp app@(ETyApp _ _) t -> fmap (++ [t]) (extract app)
                         _ -> Nothing

unpackToplevelConstr :: EType -> [EType]
unpackToplevelConstr t =
    reverse $
    flip unfoldr (Just t) $ \mT ->
        case mT of
          Nothing -> Nothing
          Just t' ->
              case t' of
                ETyApp l r ->
                    Just (r, Just l)
                _ ->
                    Just (t', Nothing)

class IsElmDefinition a where
    compileElmDef :: Proxy a -> ETypeDef

newtype SumEncoding' = SumEncoding' SumEncoding

instance Show SumEncoding' where
    show (SumEncoding' se) = case se of
                                 TaggedObject n f -> "TaggedObject " ++ show n ++ " " ++ show f
                                 ObjectWithSingleField -> "ObjectWithSingleField"
                                 TwoElemArray -> "TwoElemArray"
                                 UntaggedValue -> "UntaggedValue"

instance Eq SumEncoding' where
    SumEncoding' a == SumEncoding' b = case (a,b) of
                                           (TaggedObject a1 b1, TaggedObject a2 b2) -> a1 == a2 && b1 == b2
                                           (ObjectWithSingleField, ObjectWithSingleField) -> True
                                           (TwoElemArray, TwoElemArray) -> True
                                           (UntaggedValue, UntaggedValue) -> True
                                           _ -> False

instance Ord SumEncoding' where
    compare (SumEncoding' a) (SumEncoding' b) =
       case (a,b) of
          (TaggedObject a1 b1, TaggedObject a2 b2) -> compare a1 a2 <> compare b1 b2
          (ObjectWithSingleField, ObjectWithSingleField) -> EQ
          (TwoElemArray, TwoElemArray) -> EQ
          (UntaggedValue, UntaggedValue) -> EQ
          (TaggedObject _ _, _) -> LT
          (_, TaggedObject _ _) -> GT
          (ObjectWithSingleField, _) -> LT
          (_, ObjectWithSingleField) -> GT
          (UntaggedValue, _) -> LT
          (_, UntaggedValue) -> GT

defSumEncoding :: SumEncoding'
defSumEncoding = SumEncoding' ObjectWithSingleField

-- | Get an @elm-bridge@ type representation for a Haskell type.
-- This can be used to render the type declaration via
-- 'Elm.TyRender.ElmRenderable' or the the JSON serializer/parser names via
-- 'Elm.Json.jsonSerForType' and 'Elm.Json.jsonParserForType'.
toElmType :: (Typeable a) => Proxy a -> EType
toElmType ty = toElmType' $ typeRep ty
    where
        toElmType' :: TypeRep -> EType
        toElmType' rep
            -- String (A list of Char)
          | con == typeRepTyCon (typeRep (Proxy :: Proxy [])) &&
            args == [typeRep (Proxy :: Proxy Char)]  = ETyCon (ETCon "String")
            -- List is special because the constructor name is [] in Haskell and List in elm
          | con == typeRepTyCon (typeRep (Proxy :: Proxy [])) = ETyApp (ETyCon $ ETCon "List") (toElmType' (head args))
            -- The unit type '()' is a 0-ary tuple.
          | isTuple $ tyConName con = foldl ETyApp (ETyTuple $ length args) $ map toElmType' args
          | otherwise = typeApplication con args
            where
                (con, args) = splitTyConApp rep

        isTuple :: String -> Bool
        isTuple "Unit" = True
        isTuple ('T': 'u' : 'p': 'l' : 'e' : ds) = all Char.isDigit ds
        isTuple ('(':xs) = isTuple' $ reverse xs -- base <= 4.17
          where
            isTuple' :: String -> Bool
            isTuple' (')':xs') = all (== ',') xs'
            isTuple' _         = False
        isTuple _ = False

        typeApplication :: TyCon -> [TypeRep] -> EType
        typeApplication con args = typeApplication' (reverse args)
          where
            typeApplication' [] = ETyCon (ETCon $ tyConName con)
            typeApplication' [x] =
              ETyApp
                (ETyCon $ ETCon $ tyConName con)
                (toElmType' x)
            typeApplication' (x:xs) =
              ETyApp (typeApplication' xs) (toElmType' x)
