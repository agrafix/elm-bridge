module Elm.TyRep where

import Data.List
import Data.Proxy

import Data.Aeson.Types (SumEncoding(..))
import Data.Monoid ((<>))

class IsElmDefinition a where
    compileElmDef :: Proxy a -> ETypeDef

newtype SumEncoding' = SumEncoding' SumEncoding

instance Show SumEncoding' where
    show (SumEncoding' se) = case se of
                                 TaggedObject n f -> "TaggedObject " ++ show n ++ " " ++ show f
                                 ObjectWithSingleField -> "ObjectWithSingleField"
                                 TwoElemArray -> "TwoElemArray"

instance Eq SumEncoding' where
    SumEncoding' a == SumEncoding' b = case (a,b) of
                                           (TaggedObject a1 b1, TaggedObject a2 b2) -> a1 == a2 && b1 == b2
                                           (ObjectWithSingleField, ObjectWithSingleField) -> True
                                           (TwoElemArray, TwoElemArray) -> True
                                           _ -> False

instance Ord SumEncoding' where
    compare (SumEncoding' a) (SumEncoding' b) =
       case (a,b) of
          (TaggedObject a1 b1, TaggedObject a2 b2) -> compare a1 a2 <> compare b1 b2
          (TaggedObject _ _, _) -> LT
          (_, TaggedObject _ _) -> GT
          (ObjectWithSingleField, ObjectWithSingleField) -> EQ
          (ObjectWithSingleField, _) -> LT
          (_, ObjectWithSingleField) -> GT
          (TwoElemArray, TwoElemArray) -> EQ

defSumEncoding :: SumEncoding'
defSumEncoding = SumEncoding' ObjectWithSingleField

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
   , ea_omit_null :: Bool
   } deriving (Show, Eq, Ord)

data ESum
   = ESum
   { es_name          :: ETypeName
   , es_options       :: [(String, Either [(String, EType)] [EType])]
   , es_type          :: SumEncoding'
   , es_omit_null     :: Bool
   , es_unary_strings :: Bool
   } deriving (Show, Eq, Ord)

unpackTupleType :: EType -> [EType]
unpackTupleType t =
    unfoldr (\ty ->
                 case ty of
                   Just (ETyApp (ETyApp (ETyTuple _) r) r') -> Just (r, Just r')
                   Just tp -> Just (tp, Nothing)
                   Nothing -> Nothing
            ) (Just t)

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
