module Password exposing (Password, decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Password
    = Password String


toString : Password -> String
toString (Password str) =
    str


encode : Password -> Value
encode (Password str) =
    Encode.string str


decoder : Decoder Password
decoder =
    Decode.map Password Decode.string
