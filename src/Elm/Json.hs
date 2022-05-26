{- |
This module implements a generator for JSON serialisers and parsers of arbitrary elm types.

It is highly recommended to either only use the functions of "Elm.Module", or to use the functions in this module
after having modified the 'ETypeDef' arguments with functions such as 'defaultAlterations'.

The reason is that Elm types might have an equivalent on the Haskell side and should be converted (ie. 'Text' -> 'String', 'Vector' -> 'List').
-}
module Elm.Json
    ( jsonParserForDef
    , jsonSerForDef
    , jsonParserForType
    , jsonSerForType
    , stringSerForSimpleAdt
    , stringParserForSimpleAdt
    )
where

import           Data.Aeson.Types (SumEncoding (..))
import           Data.List
import           Elm.TyRep
import           Elm.Utils

data MaybeHandling = Root | Leaf
                   deriving Eq

-- | Compile a JSON parser for an Elm type
jsonParserForType :: EType -> String
jsonParserForType = jsonParserForType' Leaf

isOption :: EType -> Bool
isOption (ETyApp (ETyCon (ETCon "Maybe")) _) = True
isOption _                                   = False

jsonParserForType' :: MaybeHandling -> EType -> String
jsonParserForType' mh ty =
    case ty of
      ETyVar (ETVar v) -> "localDecoder_" ++ v
      ETyCon (ETCon "Int") -> "Json.Decode.int"
      ETyCon (ETCon "Float") -> "Json.Decode.float"
      ETyCon (ETCon "String") -> "Json.Decode.string"
      ETyCon (ETCon "Bool") -> "Json.Decode.bool"
      ETyCon (ETCon c) -> "jsonDec" ++ c
      ETyApp (ETyCon (ETCon "List")) t' -> "Json.Decode.list (" ++ jsonParserForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Maybe")) t' -> if mh == Root
                                                then jsonParserForType t'
                                                else "Json.Decode.maybe (" ++ jsonParserForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Set")) t' -> "decodeSet (" ++ jsonParserForType t' ++ ")"
      ETyApp (ETyApp (ETyCon (ETCon "Dict")) (ETyCon (ETCon "String")) ) value -> "Json.Decode.dict (" ++ jsonParserForType value ++ ")"
      ETyApp (ETyApp (ETyCon (ETCon "Dict")) key) value -> "decodeMap (" ++ jsonParserForType key ++ ") (" ++ jsonParserForType value ++ ")"
      _ ->
          case unpackTupleType ty of
            [] -> error $ "This should never happen. Failed to unpackTupleType: " ++ show ty
            [x] ->
                case unpackToplevelConstr x of
                  (y : ys) ->
                      jsonParserForType y ++ " "
                      ++ unwords (map (\t' -> "(" ++ jsonParserForType t' ++ ")" ) ys)
                  _ -> error $ "Do suitable json parser found for " ++ show ty
            xs ->
                let tupleLen = length xs
                in "Json.Decode.map" ++ show tupleLen ++ " tuple" ++ show tupleLen ++ " "
                    ++ unwords (zipWith (\i t' -> "(Json.Decode.index " ++ show (i :: Int) ++ " (" ++ jsonParserForType t' ++ "))") [0..] xs)

parseRecords :: Maybe ETypeName -> Bool -> [(String, EType)] -> [String]
parseRecords newtyped unwrap fields =
      case fields of
        [(_, ftype)] | unwrap -> [ succeed ++ " |> custom (" ++ jsonParserForType' (o ftype) ftype ++ ")" ]
        _ -> succeed : map mkField fields
    where
        succeed = "   Json.Decode.succeed (\\" ++ unwords (map ( ('p':) . fst ) fields) ++ " -> " ++ mkNewtype ("{" ++ intercalate ", " (map (\(fldName, _) -> fixReserved fldName ++ " = p" ++ fldName) fields) ++ "}") ++ ")"
        mkNewtype x = case newtyped of
                          Nothing -> x
                          Just nm -> "(" ++ et_name nm ++ " " ++ x ++ ")"
        o fldType = if isOption fldType
                      then Root
                      else Leaf
        mkField (fldName, fldType) =
           "   |> " ++ (if isOption fldType then "fnullable " else "required ")
                    ++ show fldName
                    ++ " (" ++ jsonParserForType' (o fldType) fldType ++ ")"

-- | Checks that all the arguments to the ESum are unary values
allUnaries :: Bool -> [SumTypeConstructor] -> Maybe [(String, String)]
allUnaries False = const Nothing
allUnaries True  = mapM isUnary
    where
        isUnary (STC o c (Anonymous args)) = if null args then Just (o,c) else Nothing
        isUnary _ = Nothing

-- | Compile a JSON parser for an Elm type definition
jsonParserForDef :: ETypeDef -> String
jsonParserForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) -> unlines
          [ decoderType name
          , makeName name ++  " ="
          , "    " ++ jsonParserForType ty
          ]
      ETypeAlias (EAlias name fields _ newtyping unwrap) -> unlines
          ( decoderType name
          : (makeName name ++ " =")
          : parseRecords (if newtyping then Just name else Nothing) unwrap fields
          )
      ETypeSum (ESum name opts (SumEncoding' encodingType) _ unarystring) ->
            decoderType name ++ "\n" ++
            makeName name ++ " =" ++
                case allUnaries unarystring opts of
                    Just names -> " " ++ deriveUnaries names
                    Nothing    -> "\n" ++ encodingDictionary opts ++ isObjectSet ++ "\n" ++ declLine opts ++ "\n"
          where
            tab n s = replicate n ' ' ++ s
            typename = et_name name
            declLine [_] = ""
            declLine _   = "    in  " ++ case encodingType of
                           ObjectWithSingleField -> unwords [ "decodeSumObjectWithSingleField ", show typename, dictName]
                           TwoElemArray          -> unwords [ "decodeSumTwoElemArray ", show typename, dictName ]
                           TaggedObject tg el    -> unwords [ "decodeSumTaggedObject", show typename, show tg, show el, dictName, isObjectSetName ]
                           UntaggedValue         -> "Json.Decode.oneOf (Dict.values " ++ dictName ++ ")"
            dictName = "jsonDecDict" ++ typename
            isObjectSetName = "jsonDecObjectSet" ++ typename
            deriveUnaries strs = unlines
                [ ""
                , "    let " ++ dictName ++ " = Dict.fromList [" ++ intercalate ", " (map (\(o, s) -> "(" ++ show s ++ ", " ++ o ++ ")") strs ) ++ "]"
                , "    in  decodeSumUnaries " ++ show typename ++ " " ++ dictName
                ]
            encodingDictionary [STC cname _ args] = "    " ++ mkDecoder cname args
            encodingDictionary os = tab 4 "let " ++ dictName ++ " = Dict.fromList\n" ++ tab 12 "[ " ++ intercalate ("\n" ++ replicate 12 ' ' ++ ", ") (map dictEntry os) ++ "\n" ++ tab 12 "]"
            isObjectSet = case encodingType of
                              TaggedObject _ _
                                | length opts > 1 ->
                                  "\n" ++ tab 8 (isObjectSetName ++ " = " ++ "Set.fromList [" ++ intercalate ", " objectSet ++ "]")
                                where objectSet =
                                        (map (show . _stcName) $ filter (isNamed . _stcFields) opts) ++
                                        -- if field is empty, it do not have content, so add to objectSet.
                                        (map (show . _stcName) $ filter (isEmpty . _stcFields) opts)
                              _ -> ""
            dictEntry (STC cname oname args) = "(" ++ show oname ++ ", " ++ mkDecoder cname args ++ ")"
            mkDecoder cname (Named args)  =  lazy $ "Json.Decode.map "
                                         ++ cname
                                         ++ " ("
                                         ++ unwords (parseRecords Nothing False args)
                                         ++ ")"

            mkDecoder cname (Anonymous args) = lazy $ unwords ( decodeFunction
                                                   : cname
                                                   : zipWith (\t' i -> "(" ++ jsonParserForIndexedType t' i ++ ")") args [0..]
                                                   )
                where decodeFunction = case length args of
                                           0 -> "Json.Decode.succeed"
                                           1 -> "Json.Decode.map"
                                           n -> "Json.Decode.map" ++ show n
                      jsonParserForIndexedType :: EType -> Int -> String
                      jsonParserForIndexedType t' i | length args <= 1 = jsonParserForType t'
                                                    | otherwise = "Json.Decode.index " ++ show i ++ " (" ++ jsonParserForType t' ++ ")"
    where
      funcname name = "jsonDec" ++ et_name name
      prependTypes str = map (\tv -> str ++ tv_name tv) . et_args
      decoderType name = funcname name ++ " : " ++ intercalate " -> " (prependTypes "Json.Decode.Decoder " name ++ [decoderTypeEnd name])
      decoderTypeEnd name = unwords ("Json.Decode.Decoder" : "(" : et_name name : map tv_name (et_args name) ++ [")"])
      makeName name = unwords (funcname name : prependTypes "localDecoder_" name)
      lazy decoder = "Json.Decode.lazy (\\_ -> " ++ decoder ++ ")"

{-| Compile a JSON serializer for an Elm type.

The 'omitNothingFields' option is currently not implemented!
-}
jsonSerForType :: EType -> String
jsonSerForType = jsonSerForType' False

jsonSerForType' :: Bool -> EType -> String
jsonSerForType' omitnull ty =
    case ty of
      ETyVar (ETVar v) -> "localEncoder_" ++ v
      ETyCon (ETCon "Int") -> "Json.Encode.int"
      ETyCon (ETCon "Float") -> "Json.Encode.float"
      ETyCon (ETCon "String") -> "Json.Encode.string"
      ETyCon (ETCon "Bool") -> "Json.Encode.bool"
      ETyCon (ETCon c) -> "jsonEnc" ++ c
      ETyApp (ETyCon (ETCon "List")) t' -> "(Json.Encode.list " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Maybe")) t' -> if omitnull
                                                then jsonSerForType t'
                                                else "(maybeEncode (" ++ jsonSerForType t' ++ "))"
      ETyApp (ETyCon (ETCon "Set")) t' -> "(encodeSet " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyApp (ETyCon (ETCon "Dict")) (ETyCon (ETCon "String"))) value -> "(Json.Encode.dict identity (" ++ jsonSerForType value ++ "))"
      ETyApp (ETyApp (ETyCon (ETCon "Dict")) key) value -> "(encodeMap (" ++ jsonSerForType key ++ ") (" ++ jsonSerForType value ++ "))"
      _ ->
          case unpackTupleType ty of
            [] -> error $ "This should never happen. Failed to unpackTupleType: " ++ show ty
            [x] ->
                case unpackToplevelConstr x of
                  (y : ys) ->
                      "(" ++ jsonSerForType y ++ " "
                      ++ unwords (map (\t' -> "(" ++ jsonSerForType t' ++ ")") ys)
                      ++ ")"
                  _ -> error $ "Do suitable json serialiser found for " ++ show ty
            xs ->
                let tupleArgsV = zip xs ([1..] :: [Int])
                    tupleArgs =
                        intercalate "," $ map (\(_, v) -> "t" ++ show v) tupleArgsV
                in "(\\(" ++ tupleArgs ++ ") -> Json.Encode.list identity [" ++  intercalate "," (map (\(t', idx) -> "(" ++ jsonSerForType t' ++ ") t" ++ show idx) tupleArgsV) ++ "])"


-- | Compile a JSON serializer for an Elm type definition
jsonSerForDef :: ETypeDef -> String
jsonSerForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) ->
          makeName name False ++  " = " ++ jsonSerForType ty ++ " val\n"
      ETypeAlias (EAlias name [(fldName, fldType)] _ newtyping True) ->
          makeName name newtyping ++ " =\n   " ++ jsonSerForType fldType ++ " val." ++ fixReserved fldName
      ETypeAlias (EAlias name fields _ newtyping _) ->
          makeName name newtyping ++ " =\n   Json.Encode.object\n   ["
          ++ intercalate "\n   ," (map (\(fldName, fldType) -> " (\"" ++ fldName ++ "\", " ++ jsonSerForType fldType ++ " val." ++ fixReserved fldName ++ ")") fields)
          ++ "\n   ]\n"
      ETypeSum (ESum name opts (SumEncoding' se) _ unarystring) ->
        case allUnaries unarystring opts of
            Nothing   -> defaultEncoding opts
            Just strs -> unaryEncoding strs
          where
              encodeFunction = case se of
                                   ObjectWithSingleField -> "encodeSumObjectWithSingleField"
                                   TwoElemArray -> "encodeSumTwoElementArray"
                                   TaggedObject k c -> unwords ["encodeSumTaggedObject", show k, show c]
                                   UntaggedValue -> "encodeSumUntagged"
              defaultEncoding [STC _ oname (Anonymous args)] = unlines
                [ makeType name
                , fname name ++ " "
                    ++ unwords (map (\tv -> "localEncoder_" ++ tv_name tv) $ et_args name)
                    ++ "(" ++ cap oname  ++ " " ++ argList args ++ ") ="
                , "    " ++ mkEncodeList args
                ]
              defaultEncoding os = unlines (
                ( makeName name False ++ " =")
                : "    let keyval v = case v of"
                : map ((replicate 12 ' ' ++) . mkcase) os
                ++ [ "    " ++ unwords ["in", encodeFunction, "keyval", "val"] ]
                )
              unaryEncoding names = unlines (
                [ makeName name False ++ " ="
                , "    case val of"
                ] ++ map (\(o, n) -> replicate 8 ' ' ++ o ++ " -> Json.Encode.string " ++ show n) names
                )
              mkcase :: SumTypeConstructor -> String
              mkcase (STC cname oname (Anonymous args)) = replicate 8 ' ' ++ cap cname ++ " " ++ argList args ++ " -> (" ++ show oname ++ ", encodeValue (" ++ mkEncodeList args ++ "))"
              mkcase (STC cname oname (Named args)) = replicate 8 ' ' ++ cap cname ++ " vs -> (" ++ show oname ++ ", " ++ mkEncodeObject args ++ ")"
              argList a = unwords $ map (\i -> "v" ++ show i ) [1 .. length a]
              numargs :: (a -> String) -> [a] -> String
              numargs f = intercalate ", " . zipWith (\n a -> f a ++ " v" ++ show n)  ([1..] :: [Int])
              mkEncodeObject args = "encodeObject [" ++ intercalate ", " (map (\(n,t) -> "(" ++ show n ++ ", " ++ jsonSerForType t ++ " vs." ++ fixReserved n ++ ")") args) ++ "]"
              mkEncodeList [arg] = jsonSerForType arg ++ " v1"
              mkEncodeList args =  "Json.Encode.list identity [" ++ numargs jsonSerForType args ++ "]"
    where
      fname name = "jsonEnc" ++ et_name name
      makeType name = fname name ++ " : " ++ intercalate " -> " (map (mkLocalEncoder . tv_name) (et_args name) ++ [unwords (et_name name : map tv_name (et_args name)) , "Value"])
      mkLocalEncoder n = "(" ++ n ++ " -> Value)"
      makeName name newtyping =
           makeType name ++ "\n"
           ++ fname name ++ " "
           ++ unwords (map (\tv -> "localEncoder_" ++ tv_name tv) $ et_args name)
           ++ if newtyping
                  then " (" ++ et_name name ++ " val)"
                  else " val"

-- | Serialize a type like 'type Color = Red | Green | Blue' in a function like
--
-- > stringEncColor : Color -> String
-- > stringEncColor x =
-- >   case x of
-- >     Red -> "red"
-- >     ...
--
-- This is mainly useful for types which are used as part of query parameters and url captures.
stringSerForSimpleAdt :: ETypeDef -> String
stringSerForSimpleAdt etd =
  case etd of
    ETypeSum (ESum name opts (SumEncoding' _se) _ _unarystring) ->
      defaultEncoding opts
      where
        defaultEncoding os =
          unlines
            ((makeName name False ++ " =") : "    case val of" : map mkcase os)
        mkcase :: SumTypeConstructor -> String
        mkcase (STC cname oname (Anonymous args)) =
          replicate 8 ' '
            ++ cap cname
            ++ " "
            ++ argList args
            ++ " -> "
            ++ show oname
        mkcase _ =
          error "stringSerForSimpleAdt.mkcase: Expecting an Anonymous case"
        argList a = unwords $ map (\i -> "v" ++ show i) [1 .. length a]
    _ -> error "stringSerForSimpleAdt only works with ETypeSum"
  where
    fname name = "stringEnc" ++ et_name name
    makeType name =
      fname name
        ++ " : "
        ++ intercalate
          " -> "
          ([unwords (et_name name : map tv_name (et_args name))] ++ ["String"])
    makeName name newtyping =
      makeType name
        ++ "\n"
        ++ fname name
        ++ " "
        ++ unwords (map (\tv -> "localEncoder_" ++ tv_name tv) $ et_args name)
        ++ if newtyping
          then " (" ++ et_name name ++ " val)"
          else " val"

-- | Parse a String into a maybe-value for simple ADT types. See 'stringSerForSimpleAdt' for motivation
stringParserForSimpleAdt :: ETypeDef -> String
stringParserForSimpleAdt etd =
  case etd of
    ETypeSum (ESum name opts (SumEncoding' _encodingType) _ _unarystring) ->
      decoderType name
        ++ "\n"
        ++ makeName name
        ++ " s =\n"
        ++ encodingDictionary opts
        ++ "\n"
      where
        tab n s = replicate n ' ' ++ s
        encodingDictionary [STC cname _ args] =
          "    " ++ mkDecoder cname args
        encodingDictionary os =
          "    case s of\n"
            ++ tab 8 ""
            ++ intercalate ("\n" ++ replicate 8 ' ') (map dictEntry os)
            ++ "\n"
            ++ tab 8 "_ -> Nothing"
        dictEntry (STC cname oname _args) =
          show oname ++ " -> Just " ++ cname
        mkDecoder _cname _ = error "impossible!"
    _ -> error "impossible"
  where
    funcname name = "stringDec" ++ et_name name
    prependTypes str = map (\tv -> str ++ tv_name tv) . et_args
    decoderType name =
      funcname name
        ++ " : "
        ++ intercalate " -> " (["String"] ++ [decoderTypeEnd name])
    decoderTypeEnd name =
      unwords ("Maybe" : et_name name : map tv_name (et_args name))
    makeName name = unwords (funcname name : prependTypes "localDecoder_" name)
