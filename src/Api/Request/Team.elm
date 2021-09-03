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
        , teamDecoder
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


teamConfirmedGet : Int -> (WebData Bool -> msg) -> Cmd msg
teamConfirmedGet teamId msg =
    get (unwrap <| teamConfirmed teamId) msg <| Decode.bool


teamJoinPut : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamJoinPut courseId teamId msg =
    put (unwrap <| teamJoin courseId)
        (Http.jsonBody (Encode.int teamId))
        msg
    <|
        teamDecoder


teamFormPost : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamFormPost courseId userId msg =
    post (unwrap <| teamForm courseId)
        (Http.jsonBody (Encode.int userId))
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


teamUserConfirmedGet : Int -> (WebData Bool -> msg) -> Cmd msg
teamUserConfirmedGet courseId msg =
    get (unwrap <| teamUserConfirmed courseId) msg <| Decode.bool


teamUserConfirmedPut : Int -> (WebData Bool -> msg) -> Cmd msg
teamUserConfirmedPut courseId msg =
    put (unwrap <| teamUserConfirmed courseId)
        Http.emptyBody
        msg
    <|
        Decode.bool


teamGet : Int -> (WebData Team -> msg) -> Cmd msg
teamGet courseId msg =
    get (unwrap <| team courseId) msg <| teamDecoder
