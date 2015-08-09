{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Elm.Derive
    ( deriveElmDef, defaultOpts, DeriveOpts(..) )
where

import Elm.TyRep

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data DeriveOpts
   = DeriveOpts
   { do_fieldModifier :: String -> String
   , do_constrModifier :: String -> String
   }

defaultOpts :: DeriveOpts
defaultOpts =
    DeriveOpts
    { do_fieldModifier = id
    , do_constrModifier = id
    }

isConcreteType :: Type -> Bool
isConcreteType ty =
    case ty of
      AppT l r ->
          isConcreteType l
      ListT -> True

conCompiler :: String -> String
conCompiler s =
    case s of
      "Double" -> "Float"
      "Text" -> "String"
      "Vector" -> "List"
      _ -> s

compileType :: Type -> Q Exp
compileType ty =
    case ty of
      ListT -> [|ETyCon (ETCon "List")|]
      TupleT i -> [|ETyTuple i|]
      ConT name ->
          let n = conCompiler $ nameBase name
          in [|ETyCon (ETCon n)|]
      VarT name ->
          let n = nameBase name
          in [|ETyVar (ETVar n)|]
      SigT ty _ ->
          compileType ty
      AppT a b ->
          let a1 = compileType a
              b1 = compileType b
          in [|ETyApp $a1 $b1|]
      _ -> fail $ "Unsupported type: " ++ show ty


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

deriveAlias :: DeriveOpts -> Name -> [TyVarBndr] -> Con -> Q [Dec]
deriveAlias opts name vars c =
    case c of
      RecC _ conFields ->
          let fields = listE $ map mkField conFields
          in runDerive name vars $ \typeName ->
                [|ETypeAlias (EAlias $typeName $fields)|]
      _ ->
          fail "Can only derive records like C { v :: Int, w :: a }"
    where
      mkField :: VarStrictType -> Q Exp
      mkField (fname, _, ftype) =
          [|(fldName, $fldType)|]
          where
            fldName = do_fieldModifier opts $ nameBase fname
            fldType = compileType ftype

deriveSum :: DeriveOpts -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
deriveSum opts name vars constrs =
    runDerive name vars $ \typeName ->
        [|ETypeSum (ESum $typeName $sumOpts)|]
    where
      sumOpts =
          listE $ map mkOpt constrs
      mkOpt :: Con -> Q Exp
      mkOpt c =
          case c of
            NormalC name args ->
                let n = do_constrModifier opts $ nameBase name
                    tyArgs = listE $ map (\(_, ty) -> compileType ty) args
                in [|(n, $tyArgs)|]
            _ ->
                fail "Can only derive sum types with options like C Int a"

deriveSynonym :: DeriveOpts -> Name -> [TyVarBndr] -> Type -> Q [Dec]
deriveSynonym opts name vars otherT =
    runDerive name vars $ \typeName ->
        [|ETypePrimAlias (EPrimAlias $typeName $otherType)|]
    where
      otherType = compileType otherT

deriveElmDef :: DeriveOpts -> Name -> Q [Dec]
deriveElmDef opts name =
    do TyConI tyCon <- reify name
       case tyCon of
         DataD _ _ tyVars constrs _ ->
             case constrs of
               [] -> fail "Can not derive empty data decls"
               [x] -> deriveAlias opts name tyVars x
               _ -> deriveSum opts name tyVars constrs
         NewtypeD _ _ tyVars constr _ ->
             deriveAlias opts name tyVars constr
         TySynD _ vars otherTy ->
             deriveSynonym opts name vars otherTy
         _ -> fail "Oops, can only derive data and newtype"
