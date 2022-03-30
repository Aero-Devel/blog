module Pages.Login exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Login exposing (Params)
import Json.Encode as Encode
import Page
import Request
import Shared
import View exposing (View)
import Element exposing (..)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init          = init
        , update        = update
        , view          = view
        , subscriptions = subscriptions
        }

-- INIT

type alias UserDetails
  =
  { email     : String
  , password  : String
  , token     : String
  }


type alias
  Model = { userDetails : UserDetails
          , style       : Style
          }

type alias Style
  =
  { orange      : Color
  , green       : Color
  , red         : Color
  , yellow      : Color
  , pink        : Color
  , grey        : Color
  , white       : Color
  , borderRad   : Int
  , sidePx      : Int
  , borderWidth : Int
  , centerWidth : Int
  }

init : ( Model, Effect Msg )
init =
    (
    { userDetails = { email = ""
                    , password = ""
                    , token = ""
                    }

    , style = { orange       = (rgb255 255 172 129)
              , green        = (rgb255 205 234 192)
              , red          = (rgb255 255 146 139)
              , yellow       = (rgb255 239 233 174)
              , pink         = (rgb255 254 195 166)
              , grey         = (rgb255 87 85 85)
              , white        = (rgb255 255 255 255)

              , borderRad    = 0
              , sidePx       = 300
              , borderWidth  = 3
              , centerWidth  = 700
              }
    }
   , Effect.none
    )



-- UPDATE


type Msg
    = EmailInput String
    | PasswordInput String
    | PressedSignIn



update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EmailInput string ->
            ( updateUsrField  model {email = string , password = model.userDetails.password,  token = ""} , Effect.none )
        PasswordInput string ->
            ( updateUsrField  model {email = model.userDetails.email , password = string, token = ""} , Effect.none  )
        PressedSignIn ->
            ( model
            , Effect.fromShared <| Shared.SignIn <| {  email    = Just model.userDetails.email
                                                    , password = Just model.userDetails.password
                                                    }
            )






--authUser



updateUsrField : Model -> UserDetails -> Model
updateUsrField m ed = { m | userDetails = ed}



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
view : Model -> View Msg
view model
  =
  { title = "login"
  , body
     = [ Element.layout [width fill, height fill , Background.color model.style.white]
       (row [width fill, height fill] (display model))]
  }

display : Model -> List (Element Msg)
display model
  = [ sidePanel model
    , mainPanel model
    , sidePanel model
    ]

--

sidePanel : Model -> Element Msg
sidePanel model = let style = model.style in
  Element.column [ width (px 300)
                 , height fill
                 ]
                 [ Element.none ]


mainPanel : Model -> Element Msg
mainPanel model
  = let style = model.style in
      Element.column [width fill]
      [ row [ height fill
            , width fill
            , Background.color style.green
            , Font.color style.grey
            ][]
      , loginboxV2 model-- loginbox model
      , row [ height fill
            , width fill
            , Background.color style.red
            ] []
      ]

box : Model
      -> List (Attribute Msg)
      -> List (Attribute Msg)
      -> List (Element Msg)
      -> Element Msg
box model rowp colp content =
      row rowp [ column colp content ]

loginboxV2 : Model -> Element Msg
loginboxV2
  model = let (outerProps,innerProps) = ( [ centerY
                                          , centerX
                                          , width fill
                                          , height fill
                                          ]
                                          ,
                                          ([ width  <| px <| round <| (goldenProp * 520)
                                           , height <| px <| 520]
                                          ++ ( carvedBorder model )
                                          ++   fullyCenter
                                          ))
          in
                box model outerProps outerProps
              [ box model innerProps innerProps
                [
                -- köra gyllene snittet här med ?
                row ([width <| px 500] ++ ( fullyCenter ))
                    [ Input.email [Border.rounded 10]
                       { onChange = EmailInput
                       , placeholder = Just <| Input.placeholder [] <| text "EmailAddress"
                       , text = model.userDetails.email
                       , label = Input.labelAbove [] <| text "Email:"
                       }
                    ],
                row ([width <| px 500] ++ ( fullyCenter))
                    [ Input.currentPassword [Border.rounded 10]
                       {  onChange = PasswordInput
                        , placeholder = Just <| Input.placeholder [] <| text "Password"
                        , text = model.userDetails.password
                        , label = Input.labelAbove [] <| text "Password:"
                        , show = False
                        }
                    ],
                row [ alignRight, padding 35 ]
                    [ Input.button [ alignRight ]
                      { onPress = Just PressedSignIn
                      , label = text "sign in"
                      }
                    ]
                ]
              ]

carvedBorder : Model -> List (Attribute Msg)
carvedBorder model
  = [ Border.width 2
    , Border.color model.style.orange
    , Border.glow model.style.orange 1
    , Border.rounded 20
    ]

fullyCenter : List (Attribute Msg)
fullyCenter = [centerY, centerX]

goldenProp : Float
goldenProp = 1.6180

-- https://auth0.com/blog/creating-your-first-elm-app-part-2/
