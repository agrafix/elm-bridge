module Elm.TyRep where

import Data.List
import Data.Proxy

class IsElmDefinition a where
    compileElmDef :: Proxy a -> ETypeDef

data ETypeDef
   = ETypeAlias EAlias
   | ETypePrimAlias EPrimAlias
   | ETypeSum ESum
     deriving (Show, Eq)

data EType
   = ETyVar ETVar
   | ETyCon ETCon
   | ETyApp EType EType
   | ETyTuple Int
   deriving (Show, Eq, Ord)

data ETCon
   = ETCon
   { tc_name :: String
   } deriving (Show, Eq, Ord)

data ETVar
   = ETVar
   { tv_name :: String
   } deriving (Show, Eq, Ord)

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
   { ea_name :: ETypeName
   , ea_fields :: [(String, EType)]
   } deriving (Show, Eq, Ord)

data ESum
   = ESum
   { es_name :: ETypeName
   , es_options :: [(String, [EType])]
   } deriving (Show, Eq, Ord)

unpackTupleType :: EType -> [EType]
unpackTupleType t =
    unfoldr (\ty ->
                 case ty of
                   Just (ETyApp (ETyApp (ETyTuple i) r) r') ->
                       Just (r, Just r')
                   Just t ->
                       Just (t, Nothing)
                   Nothing ->
                       Nothing
            ) (Just t)
