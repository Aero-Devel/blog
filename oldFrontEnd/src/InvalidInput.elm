module InvalidInput exposing (InvalidInput, decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type InvalidInput
    = IllegallChar String


type Email
    = Email String


toString : InvalidInput -> String
toString (IllegallChar str) =
    str


encode : InvalidInput -> Value
encode (IllegallChar str) =
    Encode.string str


decoder : Decoder InvalidInput
decoder =
    Decode.map IllegallChar Decode.string
