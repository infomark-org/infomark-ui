module Api.Request.Team exposing
    ( teamConfirmedGet
    , teamFormPost
    , teamGet
    , teamIncompleteGet
    , teamJoinPut
    , teamLeavePut
    , teamUserConfirmedGet
    , teamUserConfirmedPut
    )

import Api.Data.Team
    exposing
        ( Team
        , TeamBool
        , teamDecoder
        , teamBoolDecoder
        )
import Api.Endpoint
    exposing
        ( team
        , teamConfirmed
        , teamForm
        , teamIncomplete
        , teamJoin
        , teamLeave
        , teamUserConfirmed
        , unwrap
        )
import Api.Helper
    exposing
        ( get
        , post
        , put
        )
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


teamIncompleteGet : Int -> (WebData (List Team) -> msg) -> Cmd msg
teamIncompleteGet courseId msg =
    get (unwrap <| teamIncomplete courseId) msg <| Decode.list teamDecoder


teamConfirmedGet : Int -> Int -> (WebData TeamBool -> msg) -> Cmd msg
teamConfirmedGet courseId teamId msg =
    get (unwrap <| teamConfirmed courseId teamId) msg <| teamBoolDecoder


teamJoinPut : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamJoinPut courseId teamId msg =
    put (unwrap <| teamJoin courseId)
        (Encode.object
        [ ("team_id", Encode.int teamId)
        ] |> Http.jsonBody
        )
        msg
    <|
        teamDecoder


teamFormPost : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamFormPost courseId userId msg =
    post (unwrap <| teamForm courseId)
        (Encode.object
        [ ("user_id", Encode.int userId)
        ] |> Http.jsonBody
        )
        msg
    <|
        teamDecoder


teamLeavePut : Int -> (WebData Team -> msg) -> Cmd msg
teamLeavePut courseId msg =
    put (unwrap <| teamLeave courseId)
        Http.emptyBody
        msg
    <|
        teamDecoder


teamUserConfirmedGet : Int -> (WebData TeamBool -> msg) -> Cmd msg
teamUserConfirmedGet courseId msg =
    get (unwrap <| teamUserConfirmed courseId) msg <| teamBoolDecoder


teamUserConfirmedPut : Int -> (WebData TeamBool -> msg) -> Cmd msg
teamUserConfirmedPut courseId msg =
    put (unwrap <| teamUserConfirmed courseId)
        Http.emptyBody
        msg
    <|
        teamBoolDecoder


teamGet : Int -> (WebData Team -> msg) -> Cmd msg
teamGet courseId msg =
    get (unwrap <| team courseId) msg <| teamDecoder
