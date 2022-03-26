module Pages.Register exposing (Model, Msg, page)

import Gen.Params.Register exposing (Params)
import Page
import Request
import Shared
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
    = SentForm
    | WroteMail
    | WrotePass
    | CompletedRegistration
    | CurrentEmailInput String
    | CurrentPasswordInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SentForm ->
            ( model, Cmd.none )

        WroteMail ->
            ( model, Cmd.none )

        WrotePass ->
            ( model, Cmd.none )

        CompletedRegistration ->
            ( model, Cmd.none )

        CurrentEmailInput string ->
            ( model, Cmd.none )

        CurrentPasswordInput string ->
            ( model, Cmd.none )






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
        div [ class "cred-page" ]
            [ div [ class "container page" ]
                  [div [ class "row" ]
                       [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                             [ h1 [ class "text-xs-center"] [text "Sign up"]
                             ,    ul [class "errors"] (List.map viewProblem model.problems)
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
