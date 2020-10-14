module Pages.Login exposing (Model, Msg(..), init, update, view)

import Api.Data.Account exposing (Account)
import Api.Data.Role exposing (Role)
import Api.Request.Auth exposing (sessionPost)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbState(..)
        , inputElement
        , rRowButton
        )
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
import Utils.Utils as Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { email : String
    , plain_password : String
    , loginProgress : WebData Role
    , errors : List Error
    , spinner : Spinner.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , plain_password = ""
      , loginProgress = NotAsked
      , errors = []
      , spinner = Spinner.init
      }
    , Cmd.none
    )


type Msg
    = NavigateTo Route
    | SetField Field String
    | Login
    | LoginResponse (WebData Role)
    | ShowToastAndRedirect Components.Toasty.Toast
    | SpinnerMsg Spinner.Msg


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        Login ->
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    let
                        account =
                            { email = Just model.email, plain_password = Just model.plain_password }
                    in
                    ( { model | loginProgress = Loading, errors = [] }, sessionPost account LoginResponse, NoUpdate )

        -- TODO: Start the web request here.
        LoginResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            "Wrong Password or Username!"

                        Http.BadStatus 422 ->
                            "Your email is not confirmed!"

                        _ ->
                            "Something went wrong"
            in
            ( { model | loginProgress = RemoteData.Failure err }
            , Cmd.none
            , ShowToast <| Components.Toasty.Error "Error" errorString
            )

        LoginResponse (RemoteData.Success role) ->
            ( model
            , Utils.perform <|
                ShowToastAndRedirect <|
                    Components.Toasty.Success "Logged in" "You are now logged in."
            , UpdateRoleAndMail role model.email
            )

        LoginResponse _ ->
            ( model, Cmd.none, NoUpdate )

        ShowToastAndRedirect toast ->
            ( model
            , Utils.perform <| NavigateTo DashboardRoute
            , ShowToast toast
            )

        SpinnerMsg spinmsg ->
            let
                spinnerModel =
                    Spinner.update spinmsg model.spinner
            in
            ( { model | spinner = spinnerModel }, Cmd.none, NoUpdate )


type alias LoginBody =
    { email : String
    , plain_password : String
    }


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
        [ div
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
                , onSubmit Login
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
                        [ text (t "page-title-login") ]

                    -- TODO: Replace with translation
                    , div [ classes [ TC.mt4 ] ] <|
                        inputElement
                            { label = (t "mail-address")
                            , placeholder = "Email"
                            , fieldType = "email"
                            , value = model.email
                            }
                            Email
                            model.errors
                            SetField
                    , div [ classes [ TC.mt3 ] ] <|
                        inputElement
                            { label = (t "password")
                            , placeholder = (t "password")
                            , fieldType = "password"
                            , value = model.plain_password
                            }
                            Password
                            model.errors
                            SetField
                    , viewLoginButtonOrSpinner model.loginProgress model
                    ]
                , div [ classes [ TC.mt3 ] ]
                    [ button
                        [ onClick <| NavigateTo RequestPasswordResetRoute
                        , Styles.linkGreyStyle
                        ]
                        [ text (t "password-lost") ]

                    -- TODO: Create password reset page
                    , button
                        [ onClick <| NavigateTo RegistrationRoute
                        , Styles.linkGreyStyle
                        ]
                        [ text (t "action-register") ]
                    ]
                ]
            ]
        ]


viewLoginButtonOrSpinner : WebData a -> Model -> Html Msg
viewLoginButtonOrSpinner status model =
    rRowButton <|
        case status of
            RemoteData.Loading ->
                PbbSpinner model.spinner

            _ ->
                PbbButton <| PbbActive "Anmelden" Login


viewLoginError : String -> Html Msg
viewLoginError error =
    div
        [ classes
            [ TC.items_center
            , TC.justify_center
            , TC.w_100
            , TC.bg_red
            , TC.white
            , TC.flex
            , TC.pa4
            , TC.absolute
            ]
        , Styles.textStyle
        ]
        [ img [ src "/images/alert-circle.svg", classes [ TC.w2, TC.mr3 ] ] []
        , text error
        ]


type Field
    = Email
    | Password


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | plain_password = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email ( Email, "Bitte gib deine E-Mail Adresse ein." )
        , ifBlank .plain_password ( Password, "Bitte gib dein Passwort ein." )
        ]
