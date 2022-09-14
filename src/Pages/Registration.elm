module Pages.Registration exposing (Model, Msg(..), init, update, view)

import Api.Data.User exposing (User)
import Api.Data.UserAccount exposing (UserAccount)
import Api.Request.Account exposing (accountPost)
import Browser.Navigation exposing (pushUrl)
import Components.CommonElements exposing (dropdownElement, inputElement)
import Components.Dialog as Dialog
import Components.Toasty
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18n
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Toasty
import Types exposing (Language(..), Translations, languageToBackendString)
import Utils.EmailHelper as UniMailChecker
import Utils.Styles as Styles
import Validate exposing (Validator, ifBlank, ifInvalidEmail, ifNotInt, validate)


type alias Model =
    { email : String
    , password : String
    , passwordRepeat : String
    , firstName : String
    , lastName : String
    , studentNumber : String
    , semester : String
    , degree : String
    , subject : String
    , registrationProgress : WebData User
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    , noUniversityEmailDialogState : Dialog.State
    }


modelToBody : SharedState -> Model -> UserAccount
modelToBody sharedState model =
    { user =
        { id = 0
        , firstname = model.firstName
        , lastname = model.lastName
        , avatarUrl = Nothing
        , email = model.email
        , studentNumber = model.studentNumber
        , semester = Maybe.withDefault 1 <| String.toInt model.semester
        , language = languageToBackendString sharedState.selectedLanguage
        , subject = model.degree ++ " " ++ model.subject
        , root = Nothing
        }
    , account =
        { email = Just model.email
        , plain_password = Just model.password
        }
    }


type Msg
    = NavigateTo Route
    | Register Bool
    | SetField Field String
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | RegistrationResponse (WebData User)
    | NoUniversityMailWarningVisible Bool
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , passwordRepeat = ""
      , firstName = ""
      , lastName = ""
      , studentNumber = ""
      , semester = ""
      , degree = "Bachelor of Science"
      , subject = "Informatik"
      , registrationProgress = NotAsked
      , errors = []
      , toasties = Toasty.initialState
      , noUniversityEmailDialogState = False
      }
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetField field value ->
            ( setField model field value, Cmd.none, NoUpdate )

        Register force ->
            updateHandleRegister sharedState model force <| validate modelValidator model

        RegistrationResponse response ->
            updateHandleRegistrationResponse sharedState model response

        ToastyMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Toasty.update Components.Toasty.config ToastyMsg subMsg model
            in
            ( newModel, newCmd, NoUpdate )

        NoUniversityMailWarningVisible state ->
            ( { model | noUniversityEmailDialogState = state }, Cmd.none, NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


updateHandleRegister : SharedState -> Model -> Bool -> Result (List Error) success -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleRegister sharedState model force validationResult =
    case validate modelValidator model of
        Err errors ->
            ( { model | errors = errors }, Cmd.none, NoUpdate )

        Ok _ ->
            case ( UniMailChecker.isInvalid model.email, force ) of
                ( True, False ) ->
                    -- Show a warning
                    ( { model | noUniversityEmailDialogState = True }, Cmd.none, NoUpdate )

                ( _, _ ) ->
                    -- In all other cases continue with registration
                    ( { model | registrationProgress = Loading, errors = [] }
                    , accountPost (modelToBody sharedState model) RegistrationResponse
                    , NoUpdate
                    )


updateHandleRegistrationResponse : SharedState -> Model -> WebData User -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleRegistrationResponse sharedState model response =
    case response of
        Success _ ->
            ( { model | registrationProgress = response }
            , pushUrl sharedState.navKey (reverseRoute LoginRoute)
            , ShowToast <| Components.Toasty.Success "Success" "Confirmation email has been sent"
            )

        Failure (Http.BadBody error) ->
            ( { model | registrationProgress = response }, pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        Failure err ->
            let
                ( newModel, newCmd ) =
                    ( { model | registrationProgress = response }, Cmd.none )
                        |> addToast
                            (Components.Toasty.Error "Error" "Failed to register")
            in
            ( newModel, newCmd, NoUpdate )

        _ ->
            ( { model | registrationProgress = response }, Cmd.none, NoUpdate )



-- TODO: Update the shared state
-- TODO notify user that he must confirm his email or talk to admins


majors : List String
majors =
    [ "Accounting and Finance"
    , "Bioinformatik"
    , "Biologie"
    , "Computational Neuroscience"
    , "Computerlinguistik"
    , "Economics and Business Administration"
    , "General Management"
    , "Geographie"
    , "Germanistik"
    , "Koreanistik"
    , "Philosophie"
    , "Physik"
    , "Psychologie"
    , "Informatik"
    , "Erweiterungsfach Informatik"
    , "International Business Administration"
    , "International Economics"
    , "Japanologie"
    , "Kognitionswissenschaft"
    , "Mathematik"
    , "Medieninformatik"
    , "Medienwissenschaften"
    , "Medizininformatik (Uni Tübingen)"
    , "Medizintechnik (Uni Stuttgart)"
    , "Nanoscience"
    , "Sinologie"
    ]


degrees : List String
degrees =
    [ "Bachelor of Science"
    , "Master of Science"
    , "Bachelor of Education"
    , "Master of Education"
    , "Bachelor of Arts"
    , "Master of Arts"
    , "PhD"
    , "LA GymPO"
    ]


subjectOption : String -> Html msg
subjectOption subject =
    option [ selected (subject == "Bachelor of Science" || subject == "Informatik"), value subject ] [ text subject ]


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
        [ noUniEmailDialog sharedState model
        , Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
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
                    [ TC.dt, TC.center ]
                ]
                [ div
                    [ classes
                        [ TC.w3
                        , TC.dib
                        , TC.mv4
                        , TC.dtc
                        , TC.v_mid
                        ]
                    ]
                    [ img [ src "/images/Logo.svg" ] []
                    ]
                , div [ classes [ TC.dtc, TC.v_mid, TC.pl3, TC.mw7, TC.center, TC.pa4, TC.black_40 ] ] [ text "Lehrstuhl für Datenbanksysteme (Prof. Torsten Grust)" ]
                ]
            , Html.form
                [ classes
                    [ TC.mw7
                    , TC.center
                    , TC.pa4
                    , TC.black_40
                    ]
                , onSubmit <| Register False
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
                        [ text (t "page-title-registration") ]
                    , div [ classes [ TC.tl, TC.bn, TC.f6 ] ]
                        -- [ div []
                        --         ([ text "Hinweis: Die Registrierung ist geschlossen. "]
                        --         ++
                        --         [ text "Kontakt: "
                        --         --, a [ classes [ TC.link ] ] [text "info1-ws2021@informatik.uni-tuebingen.de"]
                        --         , a [ classes [ TC.link ], href "mailto:info2-ws2223@informatik.uni-tuebingen.de?subject=[Informatik 2] Registrierung" ]
                        --             [text "info2-ws2223@informatik.uni-tuebingen.de"]
                        --         ])
                        -- ]
                        [ div []
                            ([ text "Hinweis: Zur Registrierung "
                             , b [ classes [] ] [ text "muss" ]
                             , text " die universitäre E-Mail-Adresse (...@student.uni-tuebingen.de) verwendet werden. "
                             ]
                                ++ [ text "Falls diese noch nicht vergeben wurde, bitte an "

                                   --, a [ classes [ TC.link ] ] [text "info1-ws2021@informatik.uni-tuebingen.de"]
                                   , a [ classes [ TC.link ], href "mailto:info1-ws2223@informatik.uni-tuebingen.de?subject=[Informatik 2] Registrierung" ]
                                        [ text "info1-ws2223@informatik.uni-tuebingen.de" ]
                                   , text " wenden. "
                                   , text "Geben Sie in diesem Fall in Ihrer E-Mail an uns die folgenden Daten an: "
                                   , text "Vorname, Nachname, Studiengang, Semester, Matrikelnummer (falls vorhanden)."
                                   ]
                            )
                        ]

                    --TODO: Replace with translation
                    , div [ classes [ TC.w_100 ] ]
                        -- GRID!
                        [ div [ classes [ TC.mt4, TC.cf, TC.ph2_ns ] ]
                            -- First Row (First name, Last Name)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
                              -- First element
                              <|
                                inputElement
                                    { label = t "form-firstname"
                                    , placeholder = t "form-firstname"
                                    , fieldType = "text"
                                    , value = model.firstName
                                    }
                                    FirstName
                                    model.errors
                                    SetField
                            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                              -- Second element
                              <|
                                inputElement
                                    { label = t "form-lastname"
                                    , placeholder = t "form-lastname"
                                    , fieldType = "text"
                                    , value = model.lastName
                                    }
                                    LastName
                                    model.errors
                                    SetField
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Second Row (Subject, Semester number)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_30_ns ] ]
                              -- First element
                              <|
                                dropdownElement
                                    { label = "Abschluss" -- (t "form-course-of-studies")

                                    -- , placeholder = "Informatik / Medieninformatik / Medizininformatik / ..."
                                    , fieldType = "text"
                                    , value = model.degree
                                    }
                                    (List.map subjectOption degrees)
                                    --[ option [ value "Informatik" ] [ text "Informatik" ] ]
                                    Degree
                                    model.errors
                                    SetField
                            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl3_ns ] ] <|
                                dropdownElement
                                    { label = t "form-course-of-studies"

                                    -- , placeholder = "Informatik / Medieninformatik / Medizininformatik / ..."
                                    , fieldType = "text"
                                    , value = model.subject
                                    }
                                    (List.map subjectOption majors)
                                    --[ option [ value "Informatik" ] [ text "Informatik" ] ]
                                    Subject
                                    model.errors
                                    SetField
                            , div [ classes [ TC.fl, TC.w_100, TC.w_20_ns, TC.pl3_ns ] ]
                              -- Second element
                              <|
                                inputElement
                                    { label = t "form-semester"
                                    , placeholder = "1"
                                    , fieldType = "number"
                                    , value = model.semester
                                    }
                                    Semester
                                    model.errors
                                    SetField
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Thrid Row (Student Number)
                            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                                inputElement
                                    { label = t "form-matriculation-number"
                                    , placeholder = "000000 (Uni Tübingen)"
                                    , fieldType = "number"
                                    , value = model.studentNumber
                                    }
                                    StudentNumber
                                    model.errors
                                    SetField
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Fourth Row (Email)
                            [ div [ classes [ TC.fl, TC.w_100 ] ] <|
                                inputElement
                                    { label = t "mail-address"
                                    , placeholder = "Email"
                                    , fieldType = "email"
                                    , value = model.email
                                    }
                                    Email
                                    model.errors
                                    SetField
                            ]
                        , div [ classes [ TC.mt3, TC.cf, TC.ph2_ns ] ]
                            -- Fifth Row (Password, Password)
                            [ div [ classes [ TC.fl, TC.w_100, TC.w_50_ns ] ]
                              -- First element
                              <|
                                inputElement
                                    { label = t "password"
                                    , placeholder = t "password"
                                    , fieldType = "password"
                                    , value = model.password
                                    }
                                    Password
                                    model.errors
                                    SetField
                            , div [ classes [ TC.fl, TC.w_100, TC.w_50_ns, TC.pl2_ns ] ]
                              -- Second element
                              <|
                                inputElement
                                    { label = t "form-repeat-password"
                                    , placeholder = "Password"
                                    , fieldType = "password"
                                    , value = model.passwordRepeat
                                    }
                                    PasswordRepeat
                                    model.errors
                                    SetField
                            ]
                        ]
                    , div [ classes [ TC.tl, TC.bn, TC.f6 ] ]
                        [ div []
                            [ text "* Studiengang oder Abschluss nicht gefunden? "
                            , text "Bitte an "

                            --, a [ classes [ TC.link ] ] [text "info1-ws2021@informatik.uni-tuebingen.de"]
                            , a [ classes [ TC.link ], href "mailto:info1-ws2223@informatik.uni-tuebingen.de?subject=[Informatik 1] Registrierung" ]
                                [ text "info1-ws2223@informatik.uni-tuebingen.de" ]
                            , text " wenden. "
                            , text "Geben Sie in diesem Fall in Ihrer E-Mail an uns den fehlenden Studiengang oder Abschluss an."
                            ]
                        ]
                    , button
                        [ Styles.buttonGreyStyle
                        , classes [ TC.mt4, TC.w_100 ]
                        ]
                        [ text (t "form-register") ]

                    -- TODO: Replace with translation
                    ]
                , div [ classes [ TC.mt3 ] ]
                    [ button [ onClick <| NavigateTo LoginRoute, Styles.linkGreyStyle ] [ text (t "form-login") ] --TODO: Replace with translation
                    ]
                ]
            ]
        ]


noUniEmailDialog : SharedState -> Model -> Html Msg
noUniEmailDialog sharedState model =
    Dialog.modalDialog div
        [ Styles.dialogOverlayStyle
        ]
        (Dialog.dialog div
            [ Styles.dialogContainerStyle
            ]
            [ div
                [ classes [ TC.w_100, TC.ph1, TC.bb, TC.bw2, TC.b__black ] ]
                [ h1 [] [ text "Unzulässige E-Mail-Adresse" ] ]
            , div
                [ classes [ TC.w_100, TC.mt4 ] ]
                [ p [ Styles.textStyle ] [ text "Bitte verwende deine E-Mail-Adresse der Universität (...@student.uni-tuebingen.de)." ]
                , div [ classes [ TC.fr, TC.mt3 ] ]
                    [ button
                        [ classes
                            []
                        , Styles.buttonRedStyle
                        , onClick <| Register True
                        ]
                        [ text "Trotzdem Registrieren" ]
                    , button
                        [ classes
                            [ TC.ml3 ]
                        , Styles.buttonGreenStyle
                        , onClick <| NoUniversityMailWarningVisible False
                        ]
                        [ text "E-Mail-Adresse ändern" ]
                    ]
                ]
            ]
        )
        model.noUniversityEmailDialogState
        noUniversityMailDialogConfig


type Field
    = Email
    | Password
    | PasswordRepeat
    | FirstName
    | LastName
    | StudentNumber
    | Semester
    | Subject
    | Degree


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Email ->
            { model | email = value }

        Password ->
            { model | password = value }

        PasswordRepeat ->
            { model | passwordRepeat = value }

        FirstName ->
            { model | firstName = value }

        LastName ->
            { model | lastName = value }

        StudentNumber ->
            { model | studentNumber = value }

        Semester ->
            { model | semester = value }

        Subject ->
            { model | subject = value }

        Degree ->
            { model | degree = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Bitte gib deine E-Mail ein." )
            , ifInvalidEmail .email (\value -> ( Email, "Die eingegebene E-Mail Addresse " ++ value ++ " ist nicht gültig." ))
            ]
        , Validate.firstError
            [ ifBlank .semester ( Semester, "Bitte gib dein Semester ein." )
            , ifNotInt .semester (\value -> ( Semester, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isZero model.semester) ( Semester, "Auch als Informatiker beginnt das erste Semester mit 1 :)." )
            , Validate.ifTrue (\model -> isNegative model.semester) ( Semester, "Es gibt keine negativen Semester." )
            ]
        , Validate.firstError
            [ ifBlank .password ( Password, "Bitte gib ein Passwort ein." ) -- TODO: Check if password is at least 7 characters long
            , Validate.ifTrue (\model -> String.length model.password < 7) ( Password, "Das Passwort muss mindestens 7 Zeichen lang sein." )
            , ifBlank .passwordRepeat ( PasswordRepeat, "Bitte gib dein Passwort erneut ein." )
            , Validate.ifFalse (\model -> model.password == model.passwordRepeat) ( Password, "Die Passwörter müssen identisch sein." )
            ]
        , ifBlank .firstName ( FirstName, "Bitte gib deinen Vornamen ein." )
        , ifBlank .lastName ( LastName, "Bitte gib deinen Nachnamen ein." )
        , Validate.firstError
            [ ifBlank .studentNumber ( StudentNumber, "Bitte gib deine Martrikelnummer ein." )
            , ifNotInt .studentNumber (\value -> ( StudentNumber, value ++ " ist keine gültige Zahl." ))
            , Validate.ifTrue (\model -> isNegative model.studentNumber) ( StudentNumber, "Matrikelnummern sind positiv." )
            ]
        , ifBlank .degree ( Degree, "Bitte gib deinen angestrebten Abschluss ein." )
        , ifBlank .subject ( Subject, "Bitte gib dein Fach ein." )
        ]


isNegative : String -> Bool
isNegative numberString =
    case String.toInt numberString of
        Just num ->
            num < 0

        Nothing ->
            True


isZero : String -> Bool
isZero numberString =
    case String.toInt numberString of
        Just num ->
            num == 0

        Nothing ->
            True


addToast : Components.Toasty.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, cmd )


noUniversityMailDialogConfig : Dialog.Config Msg
noUniversityMailDialogConfig =
    Dialog.Config
        Styles.dialogVisibleStyle
        Styles.dialogGoneStyle
        NoUniversityMailWarningVisible
        True
        NoOp
