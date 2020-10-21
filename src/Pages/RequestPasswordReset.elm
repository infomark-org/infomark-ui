module Pages.RequestPasswordReset exposing (Model, Msg(..), init, update, view)

import Api.Request.Auth exposing (requestPasswordResetPost)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements exposing (inputElement)
import Components.Toasty
import Decoders
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Time
import Toasty
import Types exposing (Language(..), Translations)
import Utils.Styles as Styles
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { email : String
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    }


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , errors = []
      , toasties = Toasty.initialState
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | SetField Field String
    | RequestReset
    | RequestResetResponse (WebData ())
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        RequestReset ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    ( { model | errors = [] }, requestPasswordResetPost model.email RequestResetResponse, NoUpdate )

        -- TODO: Start the web request here.
        RequestResetResponse (Success _) ->
            ( model
            , pushUrl sharedState.navKey (reverseRoute LoginRoute)
            , ShowToast <|
                Components.Toasty.Success "Success"
                    "A mail with a password-reset-link was sent."
            )

        RequestResetResponse (Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 422 ->
                            "Your email is not confirmed!"
                        _ ->
                            "There was a problem requesting your password reset."
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> addToast (Components.Toasty.Error "Error" errorString)
            in
            ( newModel, newCmd, NoUpdate )

        RequestResetResponse response ->
            ( model
            , Cmd.none
            , NoUpdate
            )

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        t =
            I18n.get sharedState.translations
    in
    div
        [ classes
            [ TC.db
            , TC.pv5_l
            , TC.pv3_m
            , TC.pv1
            , TC.dt
            , TC.w_100
            ]
        ]
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , div
            [ classes
                [ TC.v_mid
                , TC.dtc
                , TC.tc
                , TC.ph3
                , TC.ph4_l
                ]

            -- Center on parent
            ]
            [ div
                [ classes
                    [ TC.w3
                    , TC.dib
                    , TC.mv4
                    ]
                ]
                [ img [ src "/images/Logo.svg" ] [] ]
            , Html.form
                [ classes
                    [ TC.mw7
                    , TC.center
                    , TC.pa4
                    , TC.black_40
                    ]
                ]
                [ fieldset
                    [ classes
                        [ TC.tl
                        , TC.bn
                        ]
                    ]
                    [ legend
                        [ classes
                            [ TC.pa0
                            , TC.mb2
                            ]
                        , Styles.headerStyle
                        ]
                        [ text (t "page-title-reset") ]

                    -- TODO: Replace with translation
                    , div [ classes [ TC.mt4 ] ] <|
                        inputElement
                            { label = "Email address"
                            , placeholder = "Email"
                            , fieldType = "email"
                            , value = model.email
                            }
                            Email
                            model.errors
                            SetField
                    , button
                        [ Styles.buttonGreyStyle
                        , classes [ TC.mt4, TC.w_100 ]
                        , onClick RequestReset
                        ]
                        [ text "Reset" ]
                    ]
                , div [ classes [ TC.mt3 ] ]
                    [ button
                        [ Styles.linkGreyStyle
                        , onClick <| NavigateTo LoginRoute
                        ]
                        [ text "Ich erinnere mich doch" ]
                    ]
                ]
            ]
        ]


type Field
    = Email


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email ( Email, "Bitte gib deine E-Mail ein." )
        ]


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )
