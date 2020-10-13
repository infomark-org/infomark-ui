port module Routing.Router exposing (CurrentModel(..), Model, Msg(..), footerView, getTranslations, init, initWith, navView, navigateTo, noTabPage, pageView, tabPage, update, updateWith, view)

--(Model, Msg(..), init, pageView, update, updateHome, updateSettings, view)

import Api.Data.Account exposing (Account)
import Api.Data.Role exposing (Role)
import Api.Request.Auth as AuthRequests
import Browser
import Browser.Navigation exposing (Key)
import Components.CommonElements exposing (inputElement)
import Components.Dialog as Dialog
import Components.Toasty
import Decoders
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import I18n
import Pages.Admin as Admin
import Pages.CourseDetail as CourseDetail
import Pages.CourseEditor as CourseEditor
import Pages.Courses as Courses
import Pages.Dashboard as Dashboard
import Pages.GroupEditor as GroupEditor
import Pages.Login as Login
import Pages.MailConfirmation as MailConfirmation
import Pages.MailEditor as MailEditor
import Pages.MaterialEditor as MaterialEditor
import Pages.PasswordReset as PasswordReset
import Pages.ProfileEditor as ProfileEditor
import Pages.Registration as Registration
import Pages.RequestPasswordReset as RequestPasswordReset
import Pages.SheetDetail as SheetDetail
import Pages.SheetEditor as SheetEditor
import Pages.SubmissionGradingEditor as SubmissionGradingEditor
import Pages.Terms as Terms
import RemoteData exposing (RemoteData(..), WebData)
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Spinner
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC
import Toasty
import Types exposing (Language(..), Translations)
import Url exposing (Url)
import Utils.PersistantState as PersistantState
import Utils.Styles as Styles
import Utils.Utils as Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { currentModel : CurrentModel
    , route : Route
    , selectedLanguage : Language
    , loginDialogState : Dialog.State
    , plain_password : String -- Only used for the modal refresh login dialog
    , errors : List Error
    , toasties : Toasty.Stack Components.Toasty.Toast
    }


type CurrentModel
    = LoginModel Login.Model
    | RegistrationModel Registration.Model
    | DashboardModel Dashboard.Model
    | AdminModel Admin.Model
    | TermsOfUseModel Terms.Model
    | CoursesModel Courses.Model
    | CourseDetailModel CourseDetail.Model
    | CourseEditorModel CourseEditor.Model
    | MaterialEditorModel MaterialEditor.Model
    | SheetDetailModel SheetDetail.Model
    | SheetEditorModel SheetEditor.Model
    | SubmissionGradingEditorModel SubmissionGradingEditor.Model
    | ProfileEditorModel ProfileEditor.Model
    | MailConfirmationModel MailConfirmation.Model
    | RequestPasswordResetModel RequestPasswordReset.Model
    | PasswordResetModel PasswordReset.Model
    | MailEditorModel MailEditor.Model
    | GroupEditorModel GroupEditor.Model
    | NotFound


type Msg
    = UrlChange Url
    | NavigateTo Route
    | ToastyMsg (Toasty.Msg Components.Toasty.Toast)
    | SelectedLanguage Language
    | LoginDialogShown Bool
    | SetField Field String
    | PersistanceUpdate (Maybe PersistantState.State)
    | Login
    | LoginResponse (WebData Role)
    | Logout
    | LogoutCompleted (WebData ())
    | HandleTranslationsResponse (WebData Translations)
    | SpinnerMsg Spinner.Msg
    | UploadProgressMsg Http.Progress
    | LoginMsg Login.Msg
    | RegistrationMsg Registration.Msg
    | DashboardMsg Dashboard.Msg
    | AdminMsg Admin.Msg
    | TermsMsg Terms.Msg
    | CoursesMsg Courses.Msg
    | CourseDetailMsg CourseDetail.Msg
    | CourseEditorMsg CourseEditor.Msg
    | MaterialEditorMsg MaterialEditor.Msg
    | SheetDetailMsg SheetDetail.Msg
    | SheetEditorMsg SheetEditor.Msg
    | SubmissionGradingEditorMsg SubmissionGradingEditor.Msg
    | ProfileEditorMsg ProfileEditor.Msg
    | MailConfirmationMsg MailConfirmation.Msg
    | RequestPasswordResetMsg RequestPasswordReset.Msg
    | PasswordResetMsg PasswordReset.Msg
    | MailEditorMsg MailEditor.Msg
    | GroupEditorMsg GroupEditor.Msg
    | NoOp


init : Url -> Language -> ( Model, Cmd Msg )
init url lang =
    let
        currentRoute =
            parseUrl url
    in
    ( { currentModel = NotFound
      , route = currentRoute
      , selectedLanguage = lang
      , loginDialogState = False
      , plain_password = ""
      , errors = []
      , toasties = Toasty.initialState
      }
    , Utils.perform <| UrlChange url
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case ( msg, model.currentModel ) of
        ( UrlChange location, _ ) ->
            let
                route =
                    parseUrl location

                ( newModel, newCmd, newSharedStateUpdate ) =
                    navigateTo route sharedState model
            in
            ( { newModel | route = route }
            , newCmd
            , newSharedStateUpdate
            )

        ( NavigateTo route, _ ) ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        ( ToastyMsg subMsg, _ ) ->
            Toasty.update Components.Toasty.config ToastyMsg subMsg model
                |> Utils.flip Utils.tupleExtend NoUpdate

        ( SelectedLanguage lang, _ ) ->
            ( { model | selectedLanguage = lang }
            , getTranslations lang
            , NoUpdate
            )

        ( HandleTranslationsResponse webData, _ ) ->
            case webData of
                Success translations ->
                    ( model, Cmd.none, UpdateLanguage model.selectedLanguage translations )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        ( PersistanceUpdate state, _ ) ->
            ( model, Cmd.none, PersistantState.stateMsgToSharedStateUpdate state )

        ( Logout, _ ) ->
            ( { model | loginDialogState = False }
            , Cmd.batch
                [ AuthRequests.sessionDelete LogoutCompleted
                , PersistantState.logout
                ]
            , NoUpdate
            )

        ( LogoutCompleted (Success _), _ ) ->
            -- Go back to login
            ( model, Browser.Navigation.pushUrl sharedState.navKey (reverseRoute LoginRoute), NoUpdate )

        ( LogoutCompleted _, _ ) ->
            -- Failure, Not asked or Loading
            ( model, Cmd.none, NoUpdate )

        ( SpinnerMsg spinnerMsg, LoginModel login ) ->
            Login.update sharedState (Login.SpinnerMsg spinnerMsg) login
                |> updateWith LoginModel LoginMsg model

        ( LoginMsg loginMsg, LoginModel login ) ->
            Login.update sharedState loginMsg login
                |> updateWith LoginModel LoginMsg model

        ( RegistrationMsg registrationMsg, RegistrationModel registration ) ->
            Registration.update sharedState registrationMsg registration
                |> updateWith RegistrationModel RegistrationMsg model

        ( DashboardMsg dashboardMsg, DashboardModel dashboard ) ->
            Dashboard.update sharedState dashboardMsg dashboard
                |> updateWith DashboardModel DashboardMsg model

        ( CoursesMsg coursesMsg, CoursesModel courses ) ->
            Courses.update sharedState coursesMsg courses
                |> updateWith CoursesModel CoursesMsg model

        ( CourseEditorMsg courseEditorMsg, CourseEditorModel courseEditor ) ->
            CourseEditor.update sharedState courseEditorMsg courseEditor
                |> updateWith CourseEditorModel CourseEditorMsg model

        ( MaterialEditorMsg materialEditorMsg, MaterialEditorModel materialEditor ) ->
            MaterialEditor.update sharedState materialEditorMsg materialEditor
                |> updateWith MaterialEditorModel MaterialEditorMsg model

        ( CourseDetailMsg courseDetailMsg, CourseDetailModel courseDetail ) ->
            CourseDetail.update sharedState courseDetailMsg courseDetail
                |> updateWith CourseDetailModel CourseDetailMsg model

        ( SheetEditorMsg sheetEditorMsg, SheetEditorModel sheetEditor ) ->
            SheetEditor.update sharedState sheetEditorMsg sheetEditor
                |> updateWith SheetEditorModel SheetEditorMsg model

        ( SheetDetailMsg sheetDetailMsg, SheetDetailModel sheetDetail ) ->
            SheetDetail.update sharedState sheetDetailMsg sheetDetail
                |> updateWith SheetDetailModel SheetDetailMsg model

        ( UploadProgressMsg progress, SheetDetailModel sheetDetail ) ->
            SheetDetail.update sharedState (SheetDetail.UploadProgressMsg progress) sheetDetail
                |> updateWith SheetDetailModel SheetDetailMsg model

        ( SpinnerMsg spinnerMsg, SubmissionGradingEditorModel submissionEditor ) ->
            SubmissionGradingEditor.update sharedState (SubmissionGradingEditor.SpinnerMsg spinnerMsg) submissionEditor
                |> updateWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model

        ( SubmissionGradingEditorMsg submissionEditorMsg, SubmissionGradingEditorModel submissionEditor ) ->
            SubmissionGradingEditor.update sharedState submissionEditorMsg submissionEditor
                |> updateWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model

        ( AdminMsg adminMsg, AdminModel admin ) ->
            Admin.update sharedState adminMsg admin
                |> updateWith AdminModel AdminMsg model

        ( TermsMsg termsMsg, TermsOfUseModel terms ) ->
            Terms.update sharedState termsMsg terms
                |> updateWith TermsOfUseModel TermsMsg model

        ( ProfileEditorMsg profileEditorMsg, ProfileEditorModel profileEditor ) ->
            ProfileEditor.update sharedState profileEditorMsg profileEditor
                |> updateWith ProfileEditorModel ProfileEditorMsg model

        ( MailConfirmationMsg mailConfirmationMsg, MailConfirmationModel mailConfirmation ) ->
            MailConfirmation.update sharedState mailConfirmationMsg mailConfirmation
                |> updateWith MailConfirmationModel MailConfirmationMsg model

        ( RequestPasswordResetMsg requestPasswordResetMsg, RequestPasswordResetModel requestPasswordReset ) ->
            RequestPasswordReset.update sharedState requestPasswordResetMsg requestPasswordReset
                |> updateWith RequestPasswordResetModel RequestPasswordResetMsg model

        ( PasswordResetMsg passwordResetMsg, PasswordResetModel passwordReset ) ->
            PasswordReset.update sharedState passwordResetMsg passwordReset
                |> updateWith PasswordResetModel PasswordResetMsg model

        ( MailEditorMsg mailEditorMsg, MailEditorModel mailEditor ) ->
            MailEditor.update sharedState mailEditorMsg mailEditor
                |> updateWith MailEditorModel MailEditorMsg model

        ( GroupEditorMsg groupEditorMsg, GroupEditorModel groupEditor ) ->
            GroupEditor.update sharedState groupEditorMsg groupEditor
                |> updateWith GroupEditorModel GroupEditorMsg model

        ( LoginDialogShown state, _ ) ->
            ( { model | loginDialogState = state }, Cmd.none, NoUpdate )

        ( SetField field val, _ ) ->
            let
                newModel =
                    setField model field val
            in
            ( newModel, Cmd.none, NoUpdate )

        ( Login, _ ) ->
            let
                request =
                    modelToLoginRequest sharedState model
            in
            case validate modelValidator model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none, NoUpdate )

                Ok _ ->
                    case request of
                        Just body ->
                            ( model
                            , AuthRequests.sessionPost body LoginResponse
                            , NoUpdate
                            )

                        Nothing ->
                            update sharedState Logout model

        ( LoginResponse (Failure err), _ ) ->
            -- Failure. Show Toast
            ( { model | plain_password = "" }, Cmd.none, NoUpdate )

        ( LoginResponse (Success role), _ ) ->
            -- Success. Hide the dialog again
            ( { model | plain_password = "", loginDialogState = False }, Cmd.none, NoUpdate )

        ( _, _ ) ->
            -- Message arrived for wrong page. Ignore that
            ( model, Cmd.none, NoUpdate )


navigateTo : Route -> SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
navigateTo route sharedState model =
    case route of
        LoginRoute ->
            Login.init |> initWith LoginModel LoginMsg model NoUpdate

        RegistrationRoute ->
            Registration.init |> initWith RegistrationModel RegistrationMsg model NoUpdate

        DashboardRoute ->
            Dashboard.init sharedState |> initWith DashboardModel DashboardMsg model NoUpdate

        CoursesRoute ->
            Courses.init |> initWith CoursesModel CoursesMsg model NoUpdate

        CreateCourseRoute ->
            CourseEditor.initCreate |> initWith CourseEditorModel CourseEditorMsg model NoUpdate

        EditCourseRoute id ->
            CourseEditor.initEdit id |> initWith CourseEditorModel CourseEditorMsg model NoUpdate

        CourseDetailRoute id ->
            CourseDetail.init id |> initWith CourseDetailModel CourseDetailMsg model NoUpdate

        CreateSheetRoute courseId ->
            SheetEditor.initCreate courseId |> initWith SheetEditorModel SheetEditorMsg model NoUpdate

        EditSheetRoute courseId id ->
            SheetEditor.initEdit courseId id |> initWith SheetEditorModel SheetEditorMsg model NoUpdate

        CreateMaterialRoute courseId ->
            MaterialEditor.initCreate courseId sharedState |> initWith MaterialEditorModel MaterialEditorMsg model NoUpdate

        EditMaterialRoute courseId id ->
            MaterialEditor.initEdit courseId id sharedState |> initWith MaterialEditorModel MaterialEditorMsg model NoUpdate

        SheetDetailRoute courseId id ->
            SheetDetail.init courseId id |> initWith SheetDetailModel SheetDetailMsg model NoUpdate

        SubmissionGradingRoute courseId taskId groupId ->
            SubmissionGradingEditor.init courseId taskId groupId |> initWith SubmissionGradingEditorModel SubmissionGradingEditorMsg model NoUpdate

        AdminRoute ->
            Admin.init |> initWith AdminModel AdminMsg model NoUpdate

        TermsOfUseRoute ->
            Terms.init |> initWith TermsOfUseModel TermsMsg model NoUpdate

        ProfileEditorRoute ->
            ProfileEditor.init |> initWith ProfileEditorModel ProfileEditorMsg model NoUpdate

        MailConfirmationRoute mail token ->
            MailConfirmation.init mail token |> initWith MailConfirmationModel MailConfirmationMsg model NoUpdate

        RequestPasswordResetRoute ->
            RequestPasswordReset.init |> initWith RequestPasswordResetModel RequestPasswordResetMsg model NoUpdate

        PasswordResetRoute mail token ->
            PasswordReset.init mail token |> initWith PasswordResetModel PasswordResetMsg model NoUpdate

        MailToUsersRoute userId ->
            MailEditor.initForUser userId |> initWith MailEditorModel MailEditorMsg model NoUpdate

        MailToGroupRoute courseId groupId ->
            MailEditor.initForGroup courseId groupId |> initWith MailEditorModel MailEditorMsg model NoUpdate

        MailToCourseRoute courseId ->
            MailEditor.initForCourse courseId |> initWith MailEditorModel MailEditorMsg model NoUpdate

        EditGroupRoute courseId groupId ->
            GroupEditor.initEditFromId courseId groupId |> initWith GroupEditorModel GroupEditorMsg model NoUpdate

        CreateGroupRoute courseId ->
            GroupEditor.initCreate courseId |> initWith GroupEditorModel GroupEditorMsg model NoUpdate

        NotFoundRoute ->
            ( { model | currentModel = NotFound }
            , Cmd.none
            , NoUpdate
            )


view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        t =
            I18n.get sharedState.translations

        title =
            case model.route of
                TermsOfUseRoute ->
                    "page-title-terms"

                LoginRoute ->
                    "page-title-login"

                RegistrationRoute ->
                    "page-title-registration"

                DashboardRoute ->
                    "page-title-dashboard"

                CoursesRoute ->
                    "page-title-courses"

                CreateCourseRoute ->
                    "page-title-create-course"

                EditCourseRoute _ ->
                    "page-title-edit-course"

                CourseDetailRoute _ ->
                    "page-title-course"

                CreateSheetRoute _ ->
                    "page-title-create-sheet"

                EditSheetRoute _ _ ->
                    "page-title-edit-sheet"

                CreateMaterialRoute _ ->
                    "page-title-create-material"

                EditMaterialRoute _ _ ->
                    "page-title-edit-material"

                SheetDetailRoute _ _ ->
                    "page-title-sheet"

                SubmissionGradingRoute _ _ _ ->
                    "page-title-grade"

                AdminRoute ->
                    "page-title-admin"

                ProfileEditorRoute ->
                    "page-title-profile"

                MailConfirmationRoute _ _ ->
                    "page-title-confirm"

                RequestPasswordResetRoute ->
                    "page-title-reset"

                PasswordResetRoute _ _ ->
                    "page-title-reset"

                CreateGroupRoute _ ->
                    "page-title-group-create"

                EditGroupRoute _ _ ->
                    "page-title-group-edit"

                MailToUsersRoute _ ->
                    "page-title-email"

                MailToGroupRoute _ _ ->
                    "page-title-email"

                MailToCourseRoute _ ->
                    "page-title-email"

                NotFoundRoute ->
                    "page-title-404"

        body =
            case model.route of
                LoginRoute ->
                    noTabPage sharedState model

                RegistrationRoute ->
                    noTabPage sharedState model

                NotFoundRoute ->
                    noTabPage sharedState model

                MailConfirmationRoute _ _ ->
                    noTabPage sharedState model

                RequestPasswordResetRoute ->
                    noTabPage sharedState model

                PasswordResetRoute _ _ ->
                    noTabPage sharedState model

                TermsOfUseRoute ->
                    noTabPage sharedState model

                _ ->
                    tabPage sharedState model
    in
    { title = t "site-title" ++ " - " ++ t title
    , body =
        [ body
            |> Html.map msgMapper
        ]
    }


navView : SharedState -> Model -> Html Msg
navView sharedState model =
    let
        t =
            I18n.get sharedState.translations

        navItems =
            [ ( t "page-title-courses", Just "images/school-white.svg", NavigateTo CoursesRoute ) ]
                ++ (if sharedState.role == Just { root = True } then
                        [ ( t "page-title-admin", Just "images/database-settings-white.svg", NavigateTo AdminRoute ) ]

                    else
                        []
                   )
                ++ [ ( t "page-title-profile", Just "images/account-settings-white.svg", NavigateTo ProfileEditorRoute )
                   , ( t "action-logout", Just "images/logout-variant-white.svg", Logout )
                   ]
    in
    nav
        [ classes
            [ TC.w_100
            , TC.flex
            , TC.justify_between
            , TC.items_center
            , TC.bb
            , TC.b__white_10
            , TC.bg_dark_red
            ]
        ]
        [ input
            [ type_ "image"
            , src "/images/Logo_white.svg"
            , onClick <| NavigateTo DashboardRoute
            , classes
                [ TC.link
                , TC.pointer
                , TC.no_underline
                , TC.flex
                , TC.items_center
                , TC.pa3
                , TC.w2
                , TC.h2
                , TC.dim
                , TC.ml1
                , TC.ml4_ns
                ]
            ]
            []
        , div
            [ classes
                [ TC.flex
                , TC.pa3
                ]
            ]
            (List.map
                (\( labelText, maybeIcon, msgAction ) ->
                    div
                        [ classes [ TC.dim, TC.pointer, TC.mr1, TC.mr4_ns, TC.flex, TC.items_center ]
                        , onClick <| msgAction
                        ]
                        (case maybeIcon of
                            Just icon ->
                                [ img
                                    [ src icon
                                    , classes [ TC.w2, TC.h2, TC.mr3, TC.mr2_l ]
                                    ]
                                    []
                                , p
                                    [ classes
                                        [ TC.f5
                                        , TC.ph2
                                        , TC.white
                                        , TC.fw6
                                        , TC.tracked
                                        , TC.ttu
                                        , TC.dn
                                        , TC.dib_l
                                        ]
                                    ]
                                    [ text labelText ]
                                ]

                            Nothing ->
                                [ p
                                    [ Styles.linkWhiteStyle
                                    , classes [ TC.fw6, TC.tracked, TC.ttu ]
                                    ]
                                    [ text labelText ]
                                ]
                        )
                )
                navItems
            )
        ]


footerView : SharedState -> Model -> Html Msg
footerView sharedState model =
    let
        t =
            I18n.get sharedState.translations
    in
    footer
        [ classes
            [ TC.pv3
            , TC.ph3
            , TC.ph5_m
            , TC.ph6_l
            , TC.dark_red
            , TC.w_100
            , TC.db
            ]
        ]
        [ small
            [ classes
                [ TC.db
                , TC.tc
                , TC.f6
                ]
            , Styles.textStyle
            ]
            [ text "© 2020 "
            , b [ classes [ TC.ttu ] ]
                [ text "Lehrstuhl Computergrafik, University Tübingen" ]
            , text "., All Rights Reserved"
            ]
        , div
            [ classes
                [ TC.tc
                , TC.mt3
                , TC.f6
                ]
            ]
            [ button [ Styles.linkGreyStyle, onClick <| SelectedLanguage German ] [ text "Deutsch" ]
            , button [ Styles.linkGreyStyle, onClick <| SelectedLanguage English ] [ text "English" ]
            , button
                [ Styles.linkGreyStyle
                , onClick <|
                    NavigateTo
                        TermsOfUseRoute
                ]
                [ text "Terms of Use" ]
            ]
        ]


tabPage : SharedState -> Model -> Html Msg
tabPage sharedState model =
    main_
        [ classes
            [ TC.w_100
            , TC.bg_white
            , TC.black
            , TC.helvetica
            ]
        ]
        [ Toasty.view Components.Toasty.config
            Components.Toasty.view
            ToastyMsg
            model.toasties
        , loginDialog
            sharedState
            model
        , navView sharedState model
        , pageView sharedState model
        , footerView sharedState model
        ]


loginDialog : SharedState -> Model -> Html Msg
loginDialog sharedState model =
    Dialog.modalDialog div
        [ Styles.dialogOverlayStyle
        ]
        (Dialog.dialog div
            [ Styles.dialogContainerStyle
            ]
            [ div
                [ classes [ TC.w_100, TC.ph1, TC.bb, TC.bw2, TC.b__black ] ]
                [ h1 [] [ text "Are you still there?" ] ]
            , div
                [ classes [ TC.w_100, TC.mt4 ] ]
                [ Html.form
                    []
                    (inputElement
                        { label = "Password"
                        , placeholder = "Password"
                        , fieldType = "password"
                        , value = model.plain_password
                        }
                        Password
                        model.errors
                        SetField
                        ++ [ div [ classes [ TC.fr, TC.mt3 ] ]
                                [ button
                                    [ classes
                                        []
                                    , Styles.buttonRedStyle
                                    , onClick <| Logout
                                    ]
                                    [ text "Nah. I'm away" ]
                                , button
                                    [ classes
                                        [ TC.ml3 ]
                                    , Styles.buttonGreenStyle
                                    , onClick Login
                                    ]
                                    [ text "Login Again" ]
                                ]
                           ]
                    )
                ]
            ]
        )
        model.loginDialogState
        loginDialogConfig


noTabPage : SharedState -> Model -> Html Msg
noTabPage sharedState model =
    div
        [ classes
            [ TC.w_100
            , TC.white
            , TC.helvetica
            ]
        ]
        [ Toasty.view Components.Toasty.config Components.Toasty.view ToastyMsg model.toasties
        , pageView sharedState model
        , footerView sharedState model
        ]


pageView : SharedState -> Model -> Html Msg
pageView sharedState model =
    case model.currentModel of
        LoginModel login ->
            Login.view sharedState login
                |> Html.map LoginMsg

        RegistrationModel registration ->
            Registration.view sharedState registration
                |> Html.map RegistrationMsg

        DashboardModel dashboard ->
            Dashboard.view sharedState dashboard
                |> Html.map DashboardMsg

        CoursesModel courses ->
            Courses.view sharedState courses
                |> Html.map CoursesMsg

        CourseEditorModel courseEditor ->
            CourseEditor.view sharedState courseEditor
                |> Html.map CourseEditorMsg

        CourseDetailModel courseDetail ->
            CourseDetail.view sharedState courseDetail
                |> Html.map CourseDetailMsg

        SheetEditorModel sheetEditor ->
            SheetEditor.view sharedState sheetEditor
                |> Html.map SheetEditorMsg

        MaterialEditorModel materialEditor ->
            MaterialEditor.view sharedState materialEditor
                |> Html.map MaterialEditorMsg

        SheetDetailModel sheetDetail ->
            SheetDetail.view sharedState sheetDetail
                |> Html.map SheetDetailMsg

        SubmissionGradingEditorModel submissionEditor ->
            SubmissionGradingEditor.view sharedState submissionEditor
                |> Html.map SubmissionGradingEditorMsg

        AdminModel admin ->
            Admin.view sharedState admin
                |> Html.map AdminMsg

        ProfileEditorModel profileEditor ->
            ProfileEditor.view sharedState profileEditor
                |> Html.map ProfileEditorMsg

        MailConfirmationModel mailConfirmation ->
            MailConfirmation.view sharedState mailConfirmation
                |> Html.map MailConfirmationMsg

        RequestPasswordResetModel requestPasswordReset ->
            RequestPasswordReset.view sharedState requestPasswordReset
                |> Html.map RequestPasswordResetMsg

        PasswordResetModel passwordReset ->
            PasswordReset.view sharedState passwordReset
                |> Html.map PasswordResetMsg

        MailEditorModel mailEditor ->
            MailEditor.view sharedState mailEditor
                |> Html.map MailEditorMsg

        GroupEditorModel groupEditor ->
            GroupEditor.view sharedState groupEditor
                |> Html.map GroupEditorMsg

        TermsOfUseModel termsView ->
            Terms.view sharedState termsView
                |> Html.map TermsMsg

        NotFound ->
            div
                [ classes
                    [ TC.dtc
                    , TC.v_mid
                    , TC.tc
                    , TC.dark_red
                    , TC.ph3
                    , TC.ph4_l
                    ]
                , style "height" "80vh"
                , style "width" "100vw"
                ]
                [ h1
                    [ classes
                        [ TC.f6
                        , TC.f2_m
                        , TC.f_subheadline_l
                        , TC.fw6
                        , TC.tc
                        ]
                    ]
                    [ text "404 :(" ]
                ]


getTranslations : Language -> Cmd Msg
getTranslations language =
    let
        url =
            case language of
                English ->
                    "/translations/en.json"

                German ->
                    "/translations/de.json"
    in
    Http.get
        { url = url
        , expect = Http.expectJson (RemoteData.fromResult >> HandleTranslationsResponse) Decoders.decodeTranslations
        }


updateWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg, SharedStateUpdate ) -> ( Model, Cmd Msg, SharedStateUpdate )
updateWith toModel toMsg model ( subModel, subCmd, subSharedStateUpdate ) =
    let
        ( newModel, newCmd, newSharedState ) =
            case subSharedStateUpdate of
                RefreshLogin ->
                    -- Intercept the request if a login is needed again
                    ( { model | loginDialogState = True }, Cmd.none, NoUpdate )

                ShowToast toast ->
                    Toasty.addToastIfUnique Components.Toasty.config ToastyMsg toast ( model, Cmd.none )
                        |> Utils.flip Utils.tupleExtend NoUpdate

                _ ->
                    ( model, PersistantState.sharedStateUpdateToStorage subSharedStateUpdate, subSharedStateUpdate )
    in
    ( { newModel | currentModel = toModel subModel }
    , Cmd.batch
        [ newCmd
        , Cmd.map toMsg subCmd
        ]
    , newSharedState
    )


initWith : (subModel -> CurrentModel) -> (subMsg -> Msg) -> Model -> SharedStateUpdate -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg, SharedStateUpdate )
initWith toModel toMsg model sharedStateUpdate ( subModel, subCmd ) =
    ( { model | currentModel = toModel subModel }
    , Cmd.map toMsg subCmd
    , sharedStateUpdate
    )


type Field
    = Password


setField : Model -> Field -> String -> Model
setField model field value =
    case field of
        Password ->
            { model | plain_password = value }


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .plain_password ( Password, "Bitte gib dein Passwort ein." )
        ]


modelToLoginRequest : SharedState -> Model -> Maybe Account
modelToLoginRequest sharedState model =
    case sharedState.userMail of
        Just mail ->
            Just
                { email = Just mail
                , plain_password = Just model.plain_password
                }

        Nothing ->
            Nothing


loginDialogConfig : Dialog.Config Msg
loginDialogConfig =
    Dialog.Config
        Styles.dialogVisibleStyle
        Styles.dialogGoneStyle
        LoginDialogShown
        False
        NoOp
