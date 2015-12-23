{- |
This module implements a generator for JSON serialisers and parsers of arbitrary elm types.
Please note: It's still very hacky and might not work for all possible elm types yet.
-}
module Elm.Json
    ( jsonParserForDef
    , jsonSerForDef
    )
where

import Data.List
import Data.Either (isLeft)
import Data.Aeson.Types (SumEncoding(..))

import Elm.TyRep
import Elm.Utils

data MaybeHandling = Root | Leaf
                   deriving Eq

jsonParserForType :: EType -> String
jsonParserForType = jsonParserForType' Leaf

isOption :: EType -> Bool
isOption (ETyApp (ETyCon (ETCon "Maybe")) _) = True
isOption _ = False

-- | Compile a JSON parser for an Elm type
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
      ETyApp (ETyCon (ETCon "HashSet")) t' -> "decodeSet (" ++ jsonParserForType t' ++ ")"
      ETyApp (ETyApp (ETyCon (ETCon mmap)) key) value
        | mmap `elem` ["Map", "HashMap"] -> "decodeMap (" ++ jsonParserForType key ++ ") (" ++ jsonParserForType value ++ ")"
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
                    commas = replicate (tupleLen - 1) ','
                in "Json.Decode.tuple" ++ show tupleLen ++ " (" ++ commas ++ ") "
                    ++ unwords (map (\t' -> "(" ++ jsonParserForType t' ++ ")") xs)

parseRecords :: Maybe ETypeName -> [(String, EType)] -> [String]
parseRecords newtyped fields = map mkField fields ++ ["   Json.Decode.succeed " ++ mkNewtype ("{" ++ intercalate ", " (map (\(fldName, _) -> fixReserved fldName ++ " = p" ++ fldName) fields) ++ "}")]
    where
        mkNewtype x = case newtyped of
                          Nothing -> x
                          Just nm -> "(" ++ et_name nm ++ " " ++ x ++ ")"
        mkField (fldName, fldType) =
           let (fldStart, fldEnd, mh) = if isOption fldType
                                            then ("(Json.Decode.maybe ", ")", Root)
                                            else ("", "", Leaf)
           in   "   " ++ fldStart ++ "(\"" ++ fldName ++ "\" := "
                      ++ jsonParserForType' mh fldType
                      ++ fldEnd
                      ++ ") `Json.Decode.andThen` \\p" ++ fldName ++ " -> "

-- | Checks that all the arguments to the ESum are unary values
allUnaries :: Bool -> [(String, Either [(String, EType)] [EType])] -> Maybe [String]
allUnaries False = const Nothing
allUnaries True  = mapM isUnary
    where
        isUnary (x, Right args) = if null args then Just x else Nothing
        isUnary _ = Nothing

-- | Compile a JSON parser for an Elm type definition
jsonParserForDef :: ETypeDef -> String
jsonParserForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) -> unlines
          [ decoderType name
          , makeName name ++  " ="
          , jsonParserForType ty
          ]
      ETypeAlias (EAlias name fields _ newtyping) -> unlines
          ( decoderType name
          : (makeName name ++ " =")
          : parseRecords (if newtyping then Just name else Nothing) fields
          )
      ETypeSum (ESum name opts (SumEncoding' encodingType) _ unarystring) ->
            decoderType name ++ "\n" ++
            makeName name ++ " =" ++
                case allUnaries unarystring opts of
                    Just names -> " " ++ deriveUnaries names
                    Nothing    -> "\n" ++ encodingDictionnary ++ isObjectSet ++ "\n    in  " ++ declLine ++ "\n"
          where
            tab n s = replicate n ' ' ++ s
            typename = et_name name
            declLine = case encodingType of
                           ObjectWithSingleField -> unwords [ "decodeSumObjectWithSingleField ", show typename, dictName]
                           TwoElemArray          -> unwords [ "decodeSumTwoElemArray ", show typename, dictName ]
                           TaggedObject tg el    -> unwords [ "decodeSumTaggedObject", show typename, show tg, show el, dictName, isObjectSetName ]
            dictName = "jsonDecDict" ++ typename
            isObjectSetName = "jsonDecObjectSet" ++ typename
            deriveUnaries strs = unlines
                [ "decodeSumUnaries " ++ show typename ++ " " ++ dictName
                , dictName ++ " = Dict.fromList [" ++ intercalate ", " (map (\s -> "(" ++ show s ++ ", " ++ cap s ++ ")") strs ) ++ "]"
                ]
            encodingDictionnary = tab 4 "let " ++ dictName ++ " = Dict.fromList\n" ++ tab 12 "[ " ++ intercalate ("\n" ++ replicate 12 ' ' ++ ", ") (map dictEntry opts) ++ "\n" ++ tab 12 "]"
            isObjectSet = case encodingType of
                              TaggedObject _ _ -> "\n" ++ tab 8 (isObjectSetName ++ " = " ++ "Set.fromList [" ++ intercalate ", " (map (show . fst) $ filter (isLeft . snd) opts) ++ "]")
                              _ -> ""
            dictEntry (oname, args) = "(" ++ show oname ++ ", " ++ mkDecoder oname args ++ ")"
            mkDecoder oname (Left args)  =  "Json.Decode.map "
                                         ++ cap oname
                                         ++ " ("
                                         ++ unwords (parseRecords Nothing args)
                                         ++ ")"
            mkDecoder oname (Right args) = unwords ( decodeFunction
                                                   : cap oname
                                                   : map (\t' -> "(" ++ jsonParserForType t' ++ ")") args
                                                   )
                where decodeFunction = case length args of
                                           0 -> "Json.Decode.succeed"
                                           1 -> "Json.Decode.map"
                                           n -> "Json.Decode.tuple" ++ show n
    where
      funcname name = "jsonDec" ++ et_name name
      prependTypes str = map (\tv -> str ++ tv_name tv) . et_args
      decoderType name = funcname name ++ " : " ++ intercalate " -> " (prependTypes "Json.Decode.Decoder " name ++ [decoderTypeEnd name])
      decoderTypeEnd name = unwords ("Json.Decode.Decoder" : "(" : et_name name : map tv_name (et_args name) ++ [")"])
      makeName name = unwords (funcname name : prependTypes "localDecoder_" name)

-- | Compile a JSON serializer for an Elm type
--
-- TODO : omitting null values on serialization ..
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
      ETyApp (ETyCon (ETCon "List")) t' -> "(Json.Encode.list << List.map " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyCon (ETCon "Maybe")) t' -> if omitnull
                                                then jsonSerForType t'
                                                else "(maybeEncode (" ++ jsonSerForType t' ++ "))"
      ETyApp (ETyCon (ETCon "Set")) t' -> "(encodeSet " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyCon (ETCon "HashSet")) t' -> "(encodeSet " ++ jsonSerForType t' ++ ")"
      ETyApp (ETyApp (ETyCon (ETCon mmap)) key) value
        | mmap `elem` ["Map", "HashMap"] -> "encodeMap (" ++ jsonSerForType key ++ ") (" ++ jsonSerForType value ++ ")"
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
                        intercalate "," $ map (\(_, v) -> "v" ++ show v) tupleArgsV
                in "(\\(" ++ tupleArgs ++ ") -> Json.Encode.list [" ++  intercalate "," (map (\(t', idx) -> "(" ++ jsonSerForType t' ++ ") v" ++ show idx) tupleArgsV) ++ "])"


-- | Compile a JSON serializer for an Elm type definition
-- TODO: handle the omit null case
jsonSerForDef :: ETypeDef -> String
jsonSerForDef etd =
    case etd of
      ETypePrimAlias (EPrimAlias name ty) ->
          makeName name False ++  " = " ++ jsonSerForType ty ++ " val\n"
      ETypeAlias (EAlias name fields _ newtyping) ->
          makeName name newtyping ++ " =\n   Json.Encode.object\n   ["
          ++ intercalate "\n   ," (map (\(fldName, fldType) -> " (\"" ++ fldName ++ "\", " ++ jsonSerForType fldType ++ " val." ++ fixReserved fldName ++ ")") fields)
          ++ "\n   ]\n"
      ETypeSum (ESum name opts (SumEncoding' se) _ unarystring) ->
        case allUnaries unarystring opts of
            Nothing -> defaultEncoding
            Just strs -> unaryEncoding strs
          where
              encodeFunction = case se of
                                   ObjectWithSingleField -> "encodeSumObjectWithSingleField"
                                   TwoElemArray -> "encodeSumTwoElementArray"
                                   TaggedObject k c -> unwords ["encodeSumTaggedObject", show k, show c]
              defaultEncoding = unlines (
                ( makeName name False ++ " =")
                : "    let keyval v = case v of"
                :  (map (replicate 12 ' ' ++) (map mkcase opts))
                ++ [ "    " ++ unwords ["in", encodeFunction, "keyval", "val"] ]
                )
              unaryEncoding names = unlines (
                [ makeName name False ++ " ="
                , "    case val of"
                ] ++ map (\n -> replicate 8 ' ' ++ cap n ++ " -> Json.Encode.string " ++ show n) names
                )
              mkcase :: (String, Either [(String, EType)] [EType]) -> String
              mkcase (oname, Right args) = replicate 8 ' ' ++ cap oname ++ " " ++ argList args ++ " -> (" ++ show oname ++ ", " ++ mkEncodeList args ++ ")"
              mkcase (oname, Left args) = replicate 8 ' ' ++ cap oname ++ " vs -> (" ++ show oname ++ ", " ++ mkEncodeObject args ++ ")"
              argList a = unwords $ map (\i -> "v" ++ show i ) [1 .. length a]
              numargs :: (a -> String) -> [a] -> String
              numargs f = intercalate ", " . zipWith (\n a -> f a ++ " v" ++ show n)  ([1..] :: [Int])
              mkEncodeObject args = "encodeObject [" ++ intercalate ", " (map (\(n,t) -> "(" ++ show n ++ ", " ++ jsonSerForType t ++ " vs." ++ fixReserved n ++ ")") args) ++ "]"
              mkEncodeList [arg] = "encodeValue (" ++ jsonSerForType arg ++ " v1)"
              mkEncodeList args =  "encodeValue (Json.Encode.list [" ++ numargs jsonSerForType args ++ "])"
    where
      makeName name newtyping =
           "jsonEnc" ++ et_name name ++ " "
           ++ unwords (map (\tv -> "localEncoder_" ++ tv_name tv) $ et_args name)
           ++ if newtyping
                  then " (" ++ et_name name ++ " val)"
                  else " val"
