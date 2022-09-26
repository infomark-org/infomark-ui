module Components.Tasks.StudentView exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

{-| The non admin counter part of TaskEditor.
Can be used to upload submissions and view the
public test results.
-}

import Api.Data.Bool exposing (BoolResponse)
import Api.Data.Grade as Grade exposing (Grade)
import Api.Data.Task exposing (Task)
import Api.Data.TaskRatingResponse exposing (TaskRatingResponse)
import Api.Data.Team as Team exposing (Team)
import Api.Request.Task as TaskRequests
import Api.Request.Team as TeamRequests exposing (teamsIncompleteGet)
import Components.CommonElements
    exposing
        ( PbbButtonState(..)
        , PbbResultState(..)
        , PbbState(..)
        , dateElement
        , datesDisplayContainer
        , fileUploader
        , inputLabel
        , r1Column
        , r2Column
        , rCollapsable
        , rCollapsablePlain
        , rContainer
        , rRow
        , rRowButton
        , rRowExtraSpacing
        , renderInTerminalBox
        , renderInTextBox
        , sliderInputElement
        )
import Components.Toasty
import Debounce exposing (Debounce)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Markdown as MD
import Pages.CourseDetail exposing (Msg(..))
import RemoteData exposing (RemoteData(..), WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Tachyons exposing (classes)
import Tachyons.Classes as TC
import Time
import Toasty
import Utils.DateFormatter as DF
import Utils.Styles as Styles
import Utils.Utils exposing (delay, handleLogoutErrors, perform)


type Field
    = Rating


type Msg
    = UploadSubmission
    | UploadSubmissionResponse (WebData ())
    | UploadProgress Http.Progress
    | GetCurrentRatingResponse (WebData TaskRatingResponse)
    | GetGradeResponse (WebData Grade) -- Change return type
    | UpdateGrade
    | RateTask Field String
    | SendRating Int
    | RateResponse (WebData ())
    | ToggleCollapse
    | GotFiles File (List File)
    | Pick
    | DragEnter
    | DragLeave
    | NoOp
    | DebounceMsg Debounce.Msg
    | DownloadSubmission String
    | TeamGetResponse (WebData Team)
    | TeamConfirmed (WebData BoolResponse)


type alias Model =
    { id : Int
    , courseId : Int
    , task : Task
    , gradeResponse : WebData Grade
    , rating : Int
    , submission : Maybe File
    , uploading : WebData ()
    , uploadPercentage : Int
    , uploadDoneTime : Maybe Time.Posix
    , collapse : Bool
    , hover : Bool
    , ratingDebounce : Debounce Int
    , ownTeamRequest : WebData Team
    , teamConfirmed : WebData BoolResponse
    }


init : Int -> Task -> ( Model, Cmd Msg )
init courseId task =
    ( { id = task.id
      , courseId = courseId
      , task = task
      , gradeResponse = Loading
      , rating = 0
      , submission = Nothing
      , uploading = NotAsked
      , uploadPercentage = 0
      , uploadDoneTime = Nothing
      , collapse = True
      , hover = False
      , ratingDebounce = Debounce.init
      , ownTeamRequest = NotAsked
      , teamConfirmed = Loading
      }
    , Cmd.batch
        [ TaskRequests.taskResultGet courseId task.id GetGradeResponse
        , TaskRequests.taskRatingGet courseId task.id GetCurrentRatingResponse
        , TeamRequests.teamGet courseId TeamGetResponse
        ]
    )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 2000
    , transform = DebounceMsg
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UploadSubmission ->
            case model.submission of
                Just file ->
                    ( { model | uploading = Loading }, TaskRequests.taskSubmissionPost model.courseId model.id file UploadSubmissionResponse, NoUpdate )

                Nothing ->
                    -- Should never happen. Upload button disabled without a set file
                    ( model, Cmd.none, NoUpdate )

        UploadSubmissionResponse (Success _) ->
            ( { model
                | uploadDoneTime = sharedState.currentTime
                , uploading = Success ()
              }
            , delay 1000 UpdateGrade
            , ShowToast <| Components.Toasty.Info "Info" "Your files have been uploaded and will be validated."
            )

        UploadSubmissionResponse response ->
            let
                newModel =
                    { model | uploading = response }

                finalModel =
                    if
                        not (RemoteData.isLoading newModel.uploading)
                            && (RemoteData.isSuccess newModel.uploading
                                    || RemoteData.isFailure newModel.uploading
                               )
                    then
                        { newModel | uploadDoneTime = sharedState.currentTime }

                    else
                        newModel

                newUpdate =
                    case response of
                        Success _ ->
                            NoUpdate

                        Failure (Http.BadStatus status) ->
                            case status of
                                500 ->
                                    ShowToast <|
                                        Components.Toasty.Error "Error"
                                            "Server rejected file. Too large or no .zip file"

                                401 ->
                                    ShowToast <|
                                        Components.Toasty.Error "Error"
                                            "Your session run out"

                                _ ->
                                    NoUpdate

                        Failure Http.Timeout ->
                            ShowToast <|
                                Components.Toasty.Error "Error"
                                    "Your request timed out, make sure your internet connection is strong enough"

                        Failure Http.NetworkError ->
                            ShowToast <|
                                Components.Toasty.Error "Error"
                                    "You lost your network connection"

                        Failure _ ->
                            NoUpdate

                        _ ->
                            NoUpdate
            in
            ( finalModel, Cmd.none, newUpdate )

        UploadProgress progress ->
            let
                percentage =
                    case progress of
                        Http.Sending p ->
                            Http.fractionSent p

                        _ ->
                            0.0

                prog =
                    if model.uploading == Loading then
                        round <| (100 * percentage)

                    else
                        round <| 0
            in
            ( { model | uploadPercentage = prog }, Cmd.none, NoUpdate )

        GetCurrentRatingResponse (Success rating) ->
            ( { model | rating = rating.own_rating }, Cmd.none, NoUpdate )

        GetCurrentRatingResponse _ ->
            ( model, Cmd.none, NoUpdate )

        GetGradeResponse response ->
            updateHandleGradeResponse sharedState model response

        UpdateGrade ->
            ( model, TaskRequests.taskResultGet model.courseId model.id GetGradeResponse, NoUpdate )

        RateTask _ rating ->
            let
                toInt =
                    Maybe.withDefault 0 <| String.toInt rating

                ( debounce, cmd ) =
                    Debounce.push debounceConfig toInt model.ratingDebounce
            in
            ( { model | rating = toInt, ratingDebounce = debounce }, cmd, NoUpdate )

        SendRating rating ->
            ( model
            , TaskRequests.taskRatingPost model.courseId model.id rating RateResponse
            , ShowToast <| Components.Toasty.Success "Success" "Your rating has been changed"
            )

        RateResponse response ->
            ( model, Cmd.none, NoUpdate )

        ToggleCollapse ->
            ( { model | collapse = not model.collapse }, Cmd.none, NoUpdate )

        GotFiles file files ->
            case ( File.size file < 7340032, String.contains ".zip" <| File.name file ) of
                ( True, True ) ->
                    ( { model | hover = False, submission = Just file }, Cmd.none, NoUpdate )

                ( False, _ ) ->
                    ( { model
                        | hover = False
                        , submission = Nothing
                      }
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error"
                            "Only files with a size of up to 7mb are allowed"
                    )

                ( _, False ) ->
                    ( { model
                        | hover = False
                        , submission = Nothing
                      }
                    , Cmd.none
                    , ShowToast <|
                        Components.Toasty.Error "Error"
                            "Only zip files are allowed for submissions"
                    )

        Pick ->
            ( model, Select.files [ "application/zip" ] GotFiles, NoUpdate )

        DragEnter ->
            ( { model | hover = True }, Cmd.none, NoUpdate )

        DragLeave ->
            ( { model | hover = False }, Cmd.none, NoUpdate )

        DebounceMsg subMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast (\r -> perform <| SendRating r))
                        subMsg
                        model.ratingDebounce
            in
            ( { model | ratingDebounce = debounce }
            , cmd
            , NoUpdate
            )

        DownloadSubmission url ->
            ( model, Download.url url, NoUpdate )

        TeamGetResponse response ->
            case response of
                Success team ->
                    case team.id of
                        Nothing ->
                            ( { model | ownTeamRequest = response }, Cmd.none, NoUpdate )

                        Just id ->
                            ( { model | ownTeamRequest = response, teamConfirmed = Loading }
                            , TeamRequests.teamConfirmedGet model.courseId id TeamConfirmed
                            , NoUpdate
                            )

                Failure err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadUrl url ->
                                    "BadUrl: " ++ url

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "Networerror"

                                Http.BadStatus status ->
                                    "BadStatus: " ++ String.fromInt status

                                Http.BadBody dbg ->
                                    case Json.decodeString (Json.field "error" Json.string) dbg of
                                        Ok s ->
                                            s

                                        _ ->
                                            "BadBody"
                    in
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" errorMsg
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , ShowToast <| Components.Toasty.Error "Error" "TeamGet response failed somehow"
                    )

        TeamConfirmed response ->
            ( { model | teamConfirmed = response }, Cmd.none, NoUpdate )

        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Bool -> Html Msg
view sharedState model deadlineReached =
    let
        upl =
            case model.gradeResponse of
                Success grade ->
                    if String.contains "Fehlerhafte Abgabe!" grade.public_test_log then
                        h2 [ classes [ TC.tl, TC.bn, TC.f6, TC.dark_red ] ]
                            [ dd [ classes [ TC.ml0 ] ] [ text "Fehlerhaft hochgeladen: ", DF.dateAndTimeFormatter sharedState grade.updated_at ] ]

                    else if String.contains "submission received and will be tested" grade.public_test_log then
                        h2 [ classes [ TC.tl, TC.bn, TC.f6, TC.dark_red ] ]
                            [ dd [ classes [ TC.ml0 ] ] [ text "Validierung der Abgabe steht noch aus: ", DF.dateAndTimeFormatter sharedState grade.updated_at ] ]

                    else
                        h2 [ classes [ TC.tl, TC.bn, TC.f6, TC.dark_green ] ]
                            [ dd [ classes [ TC.ml0 ] ] [ text "Erfolgreich hochgeladen: ", DF.dateAndTimeFormatter sharedState grade.updated_at ] ]

                _ ->
                    h2 [ classes [ TC.tl, TC.bn, TC.f6, TC.dark_red ] ] [ text "Noch nichts hochgeladen." ]

        showUpl =
            case model.teamConfirmed of
                Success b ->
                    b.bool

                _ ->
                    False
    in
    if showUpl then
        rContainer <|
            rCollapsablePlain
                [ h1 [ Styles.listHeadingStyle ] [ text model.task.name ]
                , upl
                ]
                model.collapse
                ToggleCollapse
                ( "Show", "Hide" )
            <|
                [ rRow <|
                    r1Column <|
                        [ inputLabel "Submission"
                        , h5
                            [ classes
                                [ TC.normal
                                , TC.black_80
                                , TC.mt0
                                , TC.mb1
                                ]
                            ]
                            [ text "For "
                            , span [ classes [ TC.fw6 ] ] [ text "programming exercises:" ]
                            , text " Upload a "
                            , span [ classes [ TC.fw6 ] ] [ text "zip-file" ]
                            , text " that contains your solution."

                            --, text " that contains all "
                            --, span [ classes [ TC.fw6 ] ] [ text "package directories" ]
                            --, text " from the "
                            --, span [ classes [ TC.fw6 ] ] [ text "'src/'" ]
                            --, text " folder of your Eclipse project."
                            ]

                        --, h5
                        --    [ classes
                        --        [ TC.normal
                        --        , TC.black_80
                        --        , TC.mt0
                        --        , TC.mb1
                        --        ]
                        --    ]
                        --    [ text "For "
                        --    , span [ classes [ TC.fw6 ] ] [ text "text exercises:" ]
                        --    , text " Upload a "
                        --    , span [ classes [ TC.fw6 ] ] [ text "Zip-file" ]
                        --    , text " that contains a plain "
                        --    , span [ classes [ TC.fw6 ] ] [ text ".txt-file" ]
                        --    , text " with your answers."
                        --    ]
                        , fileUploader model.hover model.submission DragEnter DragLeave Pick GotFiles
                        ]
                , if model.uploading == Loading then
                    rRowButton <| PbbProgressBar model.uploadPercentage

                  else
                    let
                        success =
                            RemoteData.isSuccess model.uploading

                        failure =
                            RemoteData.isFailure model.uploading

                        filesSelected =
                            case model.submission of
                                Just _ ->
                                    True

                                Nothing ->
                                    False

                        stateShownLongEnough =
                            Maybe.map2
                                (\up cur ->
                                    Time.posixToMillis cur - Time.posixToMillis up > 1500
                                )
                                model.uploadDoneTime
                                sharedState.currentTime
                    in
                    rRow <|
                        r2Column
                            [ rRowButton <|
                                PbbButton <|
                                    if success && stateShownLongEnough == Just False then
                                        PbbResult <| PbbFailure "ZIP-Check läuft.."

                                    else if failure && stateShownLongEnough == Just False then
                                        PbbResult <| PbbFailure "Failure"

                                    else if deadlineReached then
                                        PbbDisabled "Submission closed"

                                    else if not filesSelected then
                                        PbbDisabled "Upload"

                                    else
                                        PbbActive "Upload" UploadSubmission
                            ]
                            (case model.gradeResponse of
                                Success grade ->
                                    case grade.file_url of
                                        Just url ->
                                            if String.isEmpty url then
                                                []

                                            else
                                                [ rRowButton <|
                                                    PbbButton <|
                                                        PbbActive "Download Submission" (DownloadSubmission url)
                                                ]

                                        _ ->
                                            []

                                _ ->
                                    []
                            )
                , rRow <|
                    r1Column <|
                        [ inputLabel "Test Results"
                        , renderInTerminalBox
                            (case model.gradeResponse of
                                Success grade ->
                                    grade.public_test_log

                                Loading ->
                                    "Loading"

                                _ ->
                                    "Undefined"
                            )
                        ]
                , rRow <|
                    r1Column <|
                        [ case model.gradeResponse of
                            Success grade ->
                                if String.contains "Fehlerhafte Abgabe!" grade.public_test_log then
                                    datesDisplayContainer <|
                                        dateElement "Die Abgabe wurde zuletzt fehlerhaft hochgeladen am: " <|
                                            DF.dateAndTimeFormatter sharedState grade.updated_at

                                else if String.contains "submission received and will be tested" grade.public_test_log then
                                    datesDisplayContainer <|
                                        dateElement "Validierung der Abgabe steht noch aus: " <|
                                            DF.dateAndTimeFormatter sharedState grade.updated_at

                                else
                                    datesDisplayContainer <|
                                        dateElement "Die Abgabe wurde zuletzt erfolgreich hochgeladen am: " <|
                                            DF.dateAndTimeFormatter sharedState grade.updated_at

                            _ ->
                                text <| "Bisher wurde noch keine Datei hochgeladen."
                        ]
                ]
                    ++ (case model.gradeResponse of
                            Success grade ->
                                if String.isEmpty grade.feedback then
                                    [ text "" ]

                                else if grade.plagiat then
                                    [ rRow <|
                                        r1Column <|
                                            [ inputLabel "Feedback"
                                            , renderInTextBox
                                                grade.feedback
                                                True
                                            ]
                                    , h3 [ classes [ TC.tl, TC.bn, TC.dark_red ] ]
                                        [ text "Diese Abgabe wurde als Plagiat erkannt und wird daher mit 0 Punkten bewertet." ]
                                    , h2 [ classes [ TC.pa4, TC.mt4, TC.bt, TC.bb, TC.bw2, TC.dark_red, TC.b__black ] ]
                                        [ text <|
                                            "(Bewertung: "
                                                ++ String.fromInt grade.acquired_points
                                                ++ "/"
                                                ++ String.fromInt model.task.max_points
                                                ++ " Punkte)"
                                        ]
                                    ]

                                else
                                    [ rRow <|
                                        r1Column <|
                                            [ inputLabel "Feedback"
                                            , renderInTextBox
                                                grade.feedback
                                                True
                                            ]
                                    , h2 [ classes [ TC.pa4, TC.mt4, TC.bt, TC.bb, TC.bw2, TC.dark_red, TC.b__black ] ]
                                        [ text <|
                                            String.fromInt grade.acquired_points
                                                ++ "/"
                                                ++ String.fromInt model.task.max_points
                                                ++ " Punkte"
                                        ]
                                    ]

                            _ ->
                                [ text "" ]
                       )
                    -- ++ [ rRow <|
                    --         r1Column <|
                    --             [ h2 [ classes [ TC.tl, TC.bn, TC.dark_red ] ] [ text <| "Hinweis: Fehlerhafte Abgaben werden mit 0 Punkten bewertet!" ] ]
                    --    ]
                    ++ [ rRow <|
                            r1Column <|
                                sliderInputElement
                                    { label = "Rating (1 horrible, 5 great)"
                                    , value = model.rating
                                    , min = 0
                                    , max = 5
                                    , step = 1
                                    , valueLabel =
                                        if model.rating == 0 then
                                            "Not rated"

                                        else
                                            String.fromInt model.rating
                                    }
                                    Rating
                                    []
                                    RateTask
                       ]

    else
        rContainer <|
            [ rRow
                [ div
                    [ classes
                        [ TC.w_50
                        , TC.flex
                        , TC.flex_row
                        , TC.justify_between
                        , TC.items_center
                        ]
                    ]
                    [ h1 [ Styles.listHeadingStyle ] [ text model.task.name ]
                    , upl
                    ]
                ]
            , rRow [ h2 [ classes [ TC.tl, TC.bn, TC.f6, TC.dark_red ] ] [ text "Übungsaufgaben können ausschließlich mit einem bestätigten Team abgegeben werden." ] ]
            ]


type alias Error =
    ( Field, String )


updateHandleGradeResponse : SharedState -> Model -> WebData Grade -> ( Model, Cmd Msg, SharedStateUpdate )
updateHandleGradeResponse sharedState model response =
    case response of
        Success grade ->
            if grade.public_execution_state == Grade.Finished then
                ( { model
                    | gradeResponse = response
                  }
                , Cmd.none
                , NoUpdate
                )

            else
                ( { model
                    | gradeResponse = response
                  }
                , delay 5000 UpdateGrade
                , NoUpdate
                )

        Failure err ->
            handleLogoutErrors
                model
                sharedState
                (\e ->
                    ( { model
                        | gradeResponse = response
                      }
                    , Cmd.none
                    , NoUpdate
                    )
                )
                err

        _ ->
            ( { model
                | gradeResponse = response
              }
            , Cmd.none
            , NoUpdate
            )
