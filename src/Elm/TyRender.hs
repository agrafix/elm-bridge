{-| This module should not usually be imported. -}
module Elm.TyRender where

import           Elm.TyRep
import           Elm.Utils

import           Data.List

class ElmRenderable a where
    renderElm :: a -> String

instance ElmRenderable ETypeDef where
    renderElm td =
        case td of
          ETypeAlias alias  -> renderElm alias
          ETypeSum s        -> renderElm s
          ETypePrimAlias pa -> renderElm pa

instance ElmRenderable EType where
    renderElm ty =
        case unpackTupleType ty of
          [t] -> renderSingleTy t
          xs  -> "(" ++ intercalate ", " (map renderElm xs) ++ ")"
        where
          renderApp (ETyApp l r) = renderApp l ++ " " ++ renderElm r
          renderApp x            = renderElm x
          renderSingleTy typ =
              case typ of
                ETyVar v   -> renderElm v
                ETyCon c   -> renderElm c
                ETyTuple _ -> error "Library Bug: This should never happen!"
                ETyApp l r -> "(" ++ renderApp l ++ " " ++ renderElm r ++ ")"

instance ElmRenderable ETCon where
    renderElm = tc_name

instance ElmRenderable ETVar where
    renderElm = tv_name

instance ElmRenderable ETypeName where
    renderElm tyName =
        et_name tyName ++ " " ++ unwords (map renderElm $ et_args tyName)

instance ElmRenderable EAlias where
    renderElm alias = (if ea_newtype alias then withnewtype else nonewtype) ++ body
        where
            withnewtype = "type " ++ renderElm (ea_name alias) ++ " = " ++ et_name (ea_name alias)
            nonewtype = "type alias " ++ renderElm (ea_name alias) ++ " ="
            body = "\n   { "
                ++ intercalate "\n   , " (map (\(fld, ty) -> fixReserved fld ++ ": " ++ renderElm ty) (ea_fields alias))
                ++ "\n   }\n"

instance ElmRenderable ESum where
    renderElm s =
        "type " ++ renderElm (es_name s) ++ " =\n    "
        ++ intercalate "\n    | " (map mkOpt (es_constructors s))
        ++ "\n"
        where
          mkOpt (STC name _ (Named types)) = cap name ++ " {" ++ intercalate ", " (map (\(fld, ty) -> fixReserved fld ++ ": " ++ renderElm ty) types) ++ "}"
          mkOpt (STC name _ (Anonymous types)) =
              cap name ++ " " ++ unwords (map renderElm types)

instance ElmRenderable EPrimAlias where
    renderElm pa =
        "type alias " ++ renderElm (epa_name pa) ++ " = " ++ renderElm (epa_type pa) ++ "\n"
