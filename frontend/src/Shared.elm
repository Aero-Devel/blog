module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , User
    )

import Auth as Register
import Gen.Route
import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias User =
    { email    : Maybe String
    , password : Maybe String
    }

type alias Model =
    { user  : Maybe User
    }

unwrapUser : Model -> Maybe User
unwrapUser m = m.user


type Msg
    = SignIn User
    | SignOut


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( {user = Nothing }
    , Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SignIn u ->
            ( {model | user = Just u}
            , Request.pushRoute Gen.Route.Home_ req
            )

        SignOut ->
            ( {model | user = Nothing}
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
