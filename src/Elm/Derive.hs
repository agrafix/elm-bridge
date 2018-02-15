{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-| This module should be used to derive the Elm instance alongside the
 JSON ones. The prefered usage is to convert statements such as :

> $(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)

 into:

> $(deriveBoth defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)

 Which will derive both the @aeson@ and @elm-bridge@ instances at the same
 time.
-}

module Elm.Derive
    ( -- * Options
      A.Options(..)
    , A.SumEncoding(..)
    , defaultOptions
    , defaultOptionsDropLower
      -- * Template haskell functions
    , deriveElmDef
    , deriveBoth
    )
where

import Elm.TyRep

import Control.Monad
import Data.Aeson.TH (deriveJSON, SumEncoding(..))
import qualified Data.Aeson.TH as A
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toLower)
import Control.Applicative
import Prelude

-- | Note that This default set of options is distinct from that in
-- the @aeson@ package.
defaultOptions :: A.Options
defaultOptions
  = A.defaultOptions
  { A.sumEncoding             = A.ObjectWithSingleField
  , A.fieldLabelModifier      = id
  , A.constructorTagModifier  = id
  , A.allNullaryToStringTag   = True
  , A.omitNothingFields       = False
  , A.unwrapUnaryRecords      = True
  }

unwrapUnaryRecords :: A.Options -> Bool
unwrapUnaryRecords opts = A.unwrapUnaryRecords opts

{-| This generates a default set of options. The parameter represents the
number of characters that must be dropped from the Haskell field names.
The first letter of the field is then converted to lowercase, ie:

> data Foo = Foo { _fooBarQux :: Int }
> $(deriveBoth (defaultOptionsDropLower 4) ''Foo)

Will be encoded as:

> {"barQux"=12}
-}
defaultOptionsDropLower :: Int -> A.Options
defaultOptionsDropLower n = defaultOptions { A.fieldLabelModifier = lower . drop n }
    where
        lower "" = ""
        lower (x:xs) = toLower x : xs

compileType :: Type -> Q Exp
compileType ty =
    case ty of
      ListT -> [|ETyCon (ETCon "List")|]
      TupleT i -> [|ETyTuple i|]
      VarT name ->
          let n = nameBase name
          in [|ETyVar (ETVar n)|]
      SigT ty' _ ->
          compileType ty'
      AppT a b -> [|ETyApp $(compileType a) $(compileType b)|]
      ConT name ->
          let n = nameBase name
          in  [|ETyCon (ETCon n)|]
      _ -> fail $ "Unsupported type: " ++ show ty

optSumType :: SumEncoding -> Q Exp
optSumType se =
    case se of
        TwoElemArray -> [|SumEncoding' TwoElemArray|]
        ObjectWithSingleField -> [|SumEncoding' ObjectWithSingleField|]
        TaggedObject tn cn -> [|SumEncoding' (TaggedObject tn cn)|]
        UntaggedValue -> [|SumEncoding' UntaggedValue|]

runDerive :: Name -> [TyVarBndr] -> (Q Exp -> Q Exp) -> Q [Dec]
runDerive name vars mkBody =
    liftM (:[]) elmDefInst
    where
      elmDefInst =
          instanceD (cxt [])
              (classType `appT` instanceType)
              [ funD 'compileElmDef
                         [ clause [ return WildP ] (normalB body) []
                         ]
              ]

      classType = conT ''IsElmDefinition
      instanceType = foldl appT (conT name) $ map varT argNames

      body = mkBody [|ETypeName { et_name = nameStr, et_args = $args }|]

      nameStr = nameBase name
      args =
          listE $ map mkTVar argNames
      mkTVar :: Name -> Q Exp
      mkTVar n =
          let str = nameBase n
          in [|ETVar str|]

      argNames =
          flip map vars $ \v ->
              case v of
                PlainTV tv -> tv
                KindedTV tv _ -> tv

deriveAlias :: Bool -> A.Options -> Name -> [TyVarBndr] -> [VarStrictType] -> Q [Dec]
deriveAlias isNewtype opts name vars conFields =
        runDerive name vars $ \typeName ->
                [|ETypeAlias (EAlias $typeName $fields omitNothing isNewtype unwrapUnary)|] -- default to no newtype
    where
      unwrapUnary = unwrapUnaryRecords opts
      fields = listE $ map mkField conFields
      omitNothing = A.omitNothingFields opts
      mkField :: VarStrictType -> Q Exp
      mkField (fname, _, ftype) =
          [|(fldName, $fldType)|]
          where
            fldName = A.fieldLabelModifier opts $ nameBase fname
            fldType = compileType ftype

deriveSum :: A.Options -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
deriveSum opts name vars constrs =
    runDerive name vars $ \typeName ->
        [|ETypeSum (ESum $typeName $sumOpts $sumEncOpts omitNothing allNullary)|]
    where
      allNullary = A.allNullaryToStringTag opts
      sumEncOpts = optSumType (A.sumEncoding opts)
      omitNothing = A.omitNothingFields opts
      sumOpts = listE $ map mkOpt constrs
      mkOpt :: Con -> Q Exp
      mkOpt c =
        let modifyName = A.constructorTagModifier opts . nameBase
        in case c of
            NormalC name' args ->
                let n = modifyName name'
                    tyArgs = listE $ map (\(_, ty) -> compileType ty) args
                in [|(n, Right $tyArgs)|]
            RecC name' args ->
                let n = modifyName name'
                    tyArgs = listE $ map (\(nm, _, ty) -> let nm' = A.fieldLabelModifier opts $ nameBase nm
                                                          in  [|(nm', $(compileType ty))|]) args
                in [|(n, Left $tyArgs)|]
            _ -> fail ("Can't derive this sum: " ++ show c)

deriveSynonym :: A.Options -> Name -> [TyVarBndr] -> Type -> Q [Dec]
deriveSynonym _ name vars otherT =
    runDerive name vars $ \typeName ->
        [|ETypePrimAlias (EPrimAlias $typeName $otherType)|]
    where
      otherType = compileType otherT

-- | Equivalent to running both 'deriveJSON' and 'deriveElmDef' with the
-- same options, so as to ensure the code on the Haskell and Elm size is
-- synchronized.
deriveBoth :: A.Options -> Name -> Q [Dec]
deriveBoth o n = (++) <$> deriveElmDef o n <*> deriveJSON o n

-- | Just derive the @elm-bridge@ definitions for generating the
-- serialization/deserialization code. It must be kept synchronized with
-- the Haskell code manually.
deriveElmDef :: A.Options -> Name -> Q [Dec]
deriveElmDef opts name =
    do TyConI tyCon <- reify name
       case tyCon of
         DataD _ _ tyVars _ constrs _ ->
             case constrs of
               [] -> fail "Can not derive empty data decls"
               [RecC _ conFields] -> deriveAlias False opts name tyVars conFields
               _ -> deriveSum opts name tyVars constrs
         NewtypeD [] _ [] Nothing (NormalC _ [(Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
            deriveSynonym opts name [] otherTy
         NewtypeD [] _ [] Nothing (RecC _ conFields@[(Name (OccName _) _, Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
          if A.unwrapUnaryRecords opts
            then deriveSynonym opts name [] otherTy
            else deriveAlias True opts name [] conFields
         TySynD _ vars otherTy ->
             deriveSynonym opts name vars otherTy
         _ -> fail ("Oops, can only derive data and newtype, not this: " ++ show tyCon)
