module Pages.Register exposing (Model, Msg, page)

import ElmSpa.Request as Http
import Gen.Params.Register exposing (Params)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Page
import Request
import Shared
import String exposing (trim)
import View exposing (View)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Model

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type alias Form =
    { email    : String
    , password : String
    }

type Problem
  = InvalidEmail
  | InvalidPassword
  | ConnectionTimeout

type alias Model =
    { form : Form
    , problems : List Problem
    }

-- INIT

init : ( Model, Cmd Msg )
init =
    ( {form    = { email    = ""
                 , password = ""
                 }
    , problems = []
      }
    , Cmd.none )

-- UPDATE

type Msg
    = PressedSignup
    | WroteMail
    | WrotePass
    | CompletedRegistration
    | CurrentEmailInput String
    | CurrentPasswordInput String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedSignup ->
            ( model
            , Cmd.none
            )
        WroteMail ->
            ( model
            , Cmd.none
            )
        WrotePass ->
            ( model
            , Cmd.none
            )
        CompletedRegistration ->
            ( model
            , Cmd.none
            )
        CurrentEmailInput ei ->
            let oldForm = model.form in
            ( { model | form = ( fillFormE oldForm ei ) }
            , Cmd.none )

        CurrentPasswordInput pi ->
            let oldForm = model.form in
            ( { model | form = ( fillFormP oldForm pi ) }
            , Cmd.none )

fillFormE : Form -> String -> Form
fillFormE f s = { f | email = s }

fillFormP : Form -> String -> Form
fillFormP f p = { f | password = p }

sendform : String
sendform = "asd"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> View Msg
view model =
    { title   = "Register"
    , body =
      [
        div [ class " cred-page " ]
            [ div [ class " container page" ]
                  [div [ class " row " ]
                       [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                             [ h1 [ class " text-xs-center "] [text " Sign up "]
                             ,    ul [class " errors "] (List.map viewProblem model.problems)
                             ,    viewForm model.form
                             ]
                       ]
                  ]
            ]
      ]
    }

viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit CompletedRegistration ]
        [ fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "Email"
                , onInput CurrentEmailInput
                , value form.email
                ]
                []
            ]
        , fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , type_ "password"
                , placeholder "Password"
                , onInput CurrentPasswordInput
                , value form.password
                ]
                []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right align-center" ]
            [ text "Sign up" ]
        ]

viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEmail ->
                    "InvalidEmail"
                InvalidPassword ->
                    "InvalidPassword"
                ConnectionTimeout ->
                    "Connection timeout"
    in
    li [] [ text errorMessage ]

type alias TrimmedForm
 =
 { email : String
 , password : String
 }

trimAll : Form -> TrimmedForm
trimAll form
  =
  { email = trim form.email
  , password = trim form.password
  }



-- HTTP

register : TrimmedForm -> Http.Request Viewer
register (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]
        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Api.register body Viewer.decoder

{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , withCredentials : Bool
    }
    -> Http.Request a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }


register : Http.Body -> Decoder (Cred -> a) -> Http.Request a
register body decoder =
  post Endpoint.users Nothing body (Decode.field "user" (decoderFromCred decoder))


type alias Mail = String

type Cred
    = Cred Mail String

getMail : Cred -> Mail
getMail (Cred m _) = m

getToken : Cred -> Mail
getToken (Cred _ s) = s

credentialsDecoder : Decoder Cred
credentialsDecoder =
    Decode.succeed Cred
      |> required "email"  Email.Decoder
      |> required "token"  Decode.string
