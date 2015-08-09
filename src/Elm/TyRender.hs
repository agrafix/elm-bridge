module Elm.TyRender where

import Elm.TyRep

import Data.List

class ElmRenderable a where
    renderElm :: a -> String

instance ElmRenderable ETypeDef where
    renderElm td =
        case td of
          ETypeAlias alias -> renderElm alias
          ETypeSum s -> renderElm s
          ETypePrimAlias pa -> renderElm pa

instance ElmRenderable EType where
    renderElm ty =
        case unpackTupleType ty of
          [t] -> renderSingleTy t
          xs -> "(" ++ intercalate ", " (map renderSingleTy xs) ++ ")"
        where
          renderSingleTy ty =
              case ty of
                ETyVar v -> renderElm v
                ETyCon c -> renderElm c
                ETyTuple n -> error "Library Bug: This should never happen!"
                ETyApp l r -> "(" ++ renderElm l ++ " " ++ renderElm r ++ ")"

instance ElmRenderable ETCon where
    renderElm = tc_name

instance ElmRenderable ETVar where
    renderElm = tv_name

instance ElmRenderable ETypeName where
    renderElm tyName =
        et_name tyName ++ " " ++ unwords (map renderElm $ et_args tyName)

instance ElmRenderable EAlias where
    renderElm alias =
        "type alias " ++ renderElm (ea_name alias) ++ " = \n   { "
        ++ intercalate "\n,    " (map (\(fld, ty) -> fld ++ ": " ++ renderElm ty) (ea_fields alias))
        ++ "\n   }\n"

instance ElmRenderable ESum where
    renderElm s =
        "type " ++ renderElm (es_name s) ++ " = \n    "
        ++ intercalate "\n    | " (map mkOpt (es_options s))
        ++ "\n"
        where
          mkOpt (name, types) =
              name ++ " " ++ unwords (map renderElm types)

instance ElmRenderable EPrimAlias where
    renderElm pa =
        "type alias " ++ renderElm (epa_name pa) ++ " = " ++ renderElm (epa_type pa) ++ "\n"
