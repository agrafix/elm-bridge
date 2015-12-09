module Json.Bridge.Helpers where

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode
import Json.Decode exposing ((:=), Value)

type ObjectEncoding = EObject (List (String, Value)) | EValue Value

oeValue : ObjectEncoding -> Value
oeValue x = case x of
    EObject o -> Json.Encode.object o
    EValue  v -> v

maybe : b -> (a -> b) -> Maybe a -> b
maybe n j m = case m of
    Nothing -> n
    Just a -> j a

resmapM : (a -> Result r b) -> List a -> Result r (List b)
resmapM f lst = case lst of
   [] -> Ok []
   (x :: xs) -> f x `Result.andThen` \nx -> resmapM f xs `Result.andThen` \nxs -> Ok (nx :: nxs)


decodeSumObjectWithSingleField : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumObjectWithSingleField name mapping = Json.Decode.keyValuePairs Json.Decode.value `Json.Decode.customDecoder` \lst -> case lst of
    [(key,value)] -> decodeSumFinal name key value mapping
    _ -> Err ("Can't decode " ++ name ++ ": object has too many keys")

decodeSumTwoElemArray : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumTwoElemArray name mapping = Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.value `Json.Decode.customDecoder`
    \(key, value) -> decodeSumFinal name key value mapping

decodeSumTaggedObject : String -> String -> String -> Dict String (Json.Decode.Decoder a) -> Set String -> Json.Decode.Decoder a
decodeSumTaggedObject name fieldname contentname mapping objectKeys =
    (fieldname := Json.Decode.string) `Json.Decode.andThen` \key ->
        let decoder = if Set.member key objectKeys
                         then Json.Decode.value
                         else contentname := Json.Decode.value
        in  decoder `Json.Decode.customDecoder` \value -> decodeSumFinal name key value mapping

decodeSumFinal : String -> String -> Value -> Dict String (Json.Decode.Decoder a) -> Result String a
decodeSumFinal name key value mapping =
    case Dict.get key mapping of
        Nothing -> Err ("Unknown constructor " ++ key ++ " for type " ++ name)
        Just dec -> Json.Decode.decodeValue dec value

encodeSumObjectWithSingleField : (a -> (String, ObjectEncoding)) -> a -> Value
encodeSumObjectWithSingleField mkkeyval v =
    let (key, val) = mkkeyval v
    in  Json.Encode.object [ (key, oeValue val) ]

encodeSumTwoElementArray : (a -> (String, ObjectEncoding)) -> a -> Value
encodeSumTwoElementArray mkkeyval v =
   let (key, val) = mkkeyval v
   in  Json.Encode.list [ Json.Encode.string key, oeValue val ]

encodeSumTaggedObject : String -> String -> (a -> (String, ObjectEncoding)) -> a -> Value
encodeSumTaggedObject fieldname contentname mkkeyval v =
   let (key, eval) = mkkeyval v
       kp = (fieldname, Json.Encode.string key)
   in  case eval of
        EValue  val -> Json.Encode.object [ kp, (contentname, val) ]
        EObject obj -> Json.Encode.object ( kp :: obj )

decodeSumUnaries : String -> Dict String a -> Json.Decode.Decoder a
decodeSumUnaries typename mapping = Json.Decode.string `Json.Decode.andThen` \s -> case Dict.get s mapping of
    Nothing -> Json.Decode.fail ("Could not decode " ++ typename)
    Just x -> Json.Decode.succeed x

decodeMap : Json.Decode.Decoder comparable -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
decodeMap decKey decVal =
    let decodeKeys = resmapM decodeKey
        decodeKey (k, v) = Result.map (\nk -> (nk,v)) (Json.Decode.decodeString decKey k)
    in  Json.Decode.map Dict.fromList (Json.Decode.keyValuePairs decVal `Json.Decode.customDecoder` decodeKeys)

encodeMap : (comparable -> Json.Encode.Value) -> (v -> Json.Encode.Value) -> Dict comparable v -> Json.Encode.Value
encodeMap encKey encVal =
    let encKey' x = case Json.Decode.decodeValue Json.Decode.string (encKey x) of
            Err _ -> toString x
            Ok s -> s
    in  Json.Encode.object << List.map (\(k,v) -> (encKey' k, encVal v)) << Dict.toList

