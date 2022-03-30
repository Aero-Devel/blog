module Endpoint exposing (postAccount, Account)






{-| Http.request, except it takes an Endpoint instead of a Url.
-}
import ElmSpa.Request exposing (Request)
import Http as Http exposing (..)
import HttpBuilder exposing (RequestBuilder)
import Url.Builder as UrlBuilder exposing (QueryParameter)
import Json.Decode as Decode exposing (field, map, map2, nullable, oneOf, string, value)
import Json.Encode as Encode

type alias Either e a  = Result e a -- Or ill go cray \
type Msg
  = GotAccountMsg (Either Http.Error (Account))
  | PostAccountMsg (Either Http.Error (PostAccountResponse))

{-| Get a URL to the Conduit API.
This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.
-}
type Endpoint
    = Endpoint String

baseAdr = "http://localhost:1234"
baseBackendAdr = "http://localhost:6969"

unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

-- Endpoints

type alias Account
  =
  { mail : String
  , password : String
  }


type alias Response datatype
  =
  { payload : datatype
  , status  : String
  }


type alias Token = String

type alias PostAccountResponse =
    Response (Maybe Token)




postAccountResponseDecoder : Decode.Decoder PostAccountResponse
postAccountResponseDecoder
  = map2 Response
  (field "payload" (nullable string))
  (field "status " string)


accountDecoder : Decode.Decoder Account
accountDecoder
  = map2 Account
    ( field "mail" string)
    ( field "password" string)

accountEncoder : Account -> Encode.Value
accountEncoder account
  = Encode.object
    [ ("mail"     , Encode.string account.mail)
    , ("password" , Encode.string account.password)
    ]

accountEndpoint : Endpoint
accountEndpoint = Endpoint "account"

postAccount : Account -> Cmd Msg
postAccount acc
  = postStuff
      acc
      accountEndpoint
      accountDecoder
      accountEncoder
      GotAccountMsg



{-
makeReq:
     a                               -- Item to post
    -> Endpoint                      -- Endpoint
    -> Decode.Decoder a              -- Decoder
    -> (a -> Encode.Value)           -- encoder function
    -> (Result Error a -> Msg)       -- Msg event
    -> (String -> RequestBuilder ()) -- method
    -> Cmd Msg
makeReq item endpt dec enc expMsg method =
              UrlBuilder.crossOrigin
                  baseAdr
                  [ unwrap endpt ] []
                  |> method
                  |> HttpBuilder.withJsonBody (enc item)
                  |> HttpBuilder.withTimeout 10000
                  |> HttpBuilder.withExpect (Http.expectJson expMsg dec)
                  |> HttpBuilder.request

-}
postStuff :
   a                         -- Item to post
  -> Endpoint                -- Endpoint
  -> Decode.Decoder a        -- Decoder
  -> (a -> Encode.Value)     -- encoder function
  -> (Result Error a -> Msg) -- Msg event
  -> Cmd Msg
postStuff item endpt dec enc expMsg =
    UrlBuilder.crossOrigin
        baseAdr
        [ unwrap endpt ] []
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody (enc item)
        |> HttpBuilder.withTimeout 10000
        |> HttpBuilder.withExpect (Http.expectJson expMsg dec)
        |> HttpBuilder.request





