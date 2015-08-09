module Elm.Json
    ( jsonParserForDef
    , jsonSerForDef
    )
where

import Data.List
import Data.Maybe

import Elm.TyRep

jsonParserForType :: EType -> String
jsonParserForType ty =
    case ty of
      ETyVar (ETVar v) -> "localDecoder_" ++ v
      ETyCon (ETCon "Int") -> "Json.Decode.int"
      ETyCon (ETCon "Float") -> "Json.Decode.float"
      ETyCon (ETCon "String") -> "Json.Decode.string"
      ETyCon (ETCon "Bool") -> "Json.Decode.bool"
      ETyCon (ETCon c) -> "jsonDec" ++ c
      ETyApp (ETyCon (ETCon "List")) t' -> "Json.Decode.list (" ++ jsonParserForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Maybe")) t' -> "Json.Decode.maybe (" ++ jsonParserForType t' ++ ")"
      _ ->
          case unpackTupleType ty of
            [] -> error $ "This should never happen. Failed to unpackTupleType: " ++ show ty
            [x] ->
                case unpackToplevelConstr x of
                  (y : ys) ->
                      jsonParserForType y ++ " "
                      ++ unwords (catMaybes $ map (\t' ->
                                                 case t' of
                                                   ETyVar _ -> Just $ "(" ++ jsonParserForType t' ++ ")"
                                                   _ -> Nothing
                                            ) ys)
                  _ -> error $ "Do suitable json parser found for " ++ show ty
            xs ->
                let tupleLen = length xs
                    commas = replicate (tupleLen - 1) ','
                in "Json.Decode.tuple" ++ show tupleLen ++ " (" ++ commas ++ ") "
                    ++ unwords (map (\t' -> "(" ++ jsonParserForType t' ++ ")") xs)

jsonParserForDef :: ETypeDef -> String
jsonParserForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) ->
          makeName name ++  " = " ++ jsonParserForType ty ++ "\n"
      ETypeAlias (EAlias name fields) ->
          makeName name ++ " = \n"
          ++ intercalate "\n" (map (\(fldName, fldType) -> "   (\"" ++ fldName ++ "\" := "
                                    ++ jsonParserForType fldType
                                    ++ ") `Json.Decode.andThen` \\p" ++ fldName ++ " -> ") fields)
          ++ "\n   Json.Decode.succeed {" ++ intercalate ", " (map (\(fldName, _) -> fldName ++ " = p" ++ fldName) fields) ++ "}\n"
      ETypeSum (ESum name opts) ->
          makeName name ++ " = \n"
          ++ "   Json.Decode.oneOf \n   [ "
          ++ intercalate "\n   , " (map mkOpt opts) ++ "\n"
          ++ "   ]\n"
          where
            mkOpt (name, args) =
                let argLen = length args
                in "(\"" ++ name ++ "\" := Json.tuple" ++ show argLen ++ " " ++ name ++ " "
                   ++ unwords (map (\t' -> "(" ++ jsonParserForType t' ++ ")") args)
                   ++ ")"
    where
      makeName name =
           "jsonDec" ++ et_name name ++ " "
           ++ unwords (map (\tv -> "localDecoder_" ++ tv_name tv) $ et_args name)


jsonSerForType :: EType -> String
jsonSerForType ty =
    case ty of
      ETyVar (ETVar v) -> "localEncoder_" ++ v
      ETyCon (ETCon "Int") -> "Json.Encode.int"
      ETyCon (ETCon "Float") -> "Json.Encode.float"
      ETyCon (ETCon "String") -> "Json.Encode.string"
      ETyCon (ETCon "Bool") -> "Json.Encode.bool"
      ETyCon (ETCon c) -> "jsonEnc" ++ c
      ETyApp (ETyCon (ETCon "List")) t' -> "(Json.Encode.list << map " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Maybe")) t' -> "(\v -> case v of Just val -> " ++ jsonSerForType t' ++ " val Nothing -> Json.Encode.null)"
      _ ->
          case unpackTupleType ty of
            [] -> error $ "This should never happen. Failed to unpackTupleType: " ++ show ty
            [x] ->
                case unpackToplevelConstr x of
                  (y : ys) ->
                      "(" ++ jsonSerForType y ++ " "
                      ++ unwords (catMaybes $ map (\t' ->
                                                 case t' of
                                                   ETyVar _ -> Just $ "(" ++ jsonSerForType t' ++ ")"
                                                   _ -> Nothing
                                            ) ys) ++ ")"
                  _ -> error $ "Do suitable json serialiser found for " ++ show ty
            xs ->
                let tupleLen = length xs
                    tupleArgsV = zip xs [1..]
                    tupleArgs =
                        unwords $ map (\(_, v) -> "v" ++ show v) tupleArgsV
                in "(\\" ++ tupleArgs ++ " -> [" ++  intercalate "," (map (\(t', idx) -> "(" ++ jsonSerForType t' ++ ") v" ++ show idx) tupleArgsV) ++ "]"


jsonSerForDef :: ETypeDef -> String
jsonSerForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) ->
          makeName name ++  " = " ++ jsonSerForType ty ++ " val\n"
      ETypeAlias (EAlias name fields) ->
          makeName name ++ " = \n   Json.Encode.object\n   ["
          ++ intercalate "\n   ," (map (\(fldName, fldType) -> " (\"" ++ fldName ++ "\", " ++ jsonSerForType fldType ++ " val." ++ fldName ++ ")") fields)
          ++ "\n   ]\n"
      ETypeSum (ESum name opts) ->
          makeName name ++ " = \n"
          ++ "   case val of\n   "
          ++ intercalate "\n   " (map mkOpt opts) ++ "\n"
          where
            mkOpt (name, args) =
                let namedArgs = zip args [1..]
                    argList = unwords $ map (\(_, i) -> "v" ++ show i ) namedArgs
                    mkArg :: (EType, Int) -> String
                    mkArg (arg, idx) =
                        jsonSerForType arg ++ " v" ++ show idx
                in "   " ++ name ++ " " ++ argList ++ " -> [" ++ intercalate ", " (map mkArg namedArgs) ++ "]"
    where
      makeName name =
           "jsonEnc" ++ et_name name ++ " "
           ++ unwords (map (\tv -> "localEncoder_" ++ tv_name tv) $ et_args name)
           ++ " val"
