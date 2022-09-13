module Api.Request.Team exposing (..)

import Api.Data.Bool as Bool exposing (BoolResponse, encoder)
import Api.Data.Course as Course exposing (Course)
import Api.Data.Team as Team exposing (Team, encoder)
import Api.Data.TeamForm as TeamForm exposing (TeamForm, encoder)
import Api.Endpoint
    exposing
        ( team
        , teamConfirmed
        , teamForm
        , teamJoin
        , teamLeave
        , teamUserConfirmed
        , teamsIncomplete
        , unwrap
        )
import Api.Helper exposing (get, patchExpectNothing, post, postExpectNothing, postFile, put, put2, putExpectNothing)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)


teamGet : Int -> (WebData Team -> msg) -> Cmd msg
teamGet courseId msg =
    get (unwrap <| team courseId)
        msg
        Team.decoder


teamsIncompleteGet : Int -> (WebData (List Team) -> msg) -> Cmd msg
teamsIncompleteGet courseId msg =
    get (unwrap <| teamsIncomplete courseId)
        msg
        (Decode.list Team.decoder)


teamConfirmedGet : Int -> Int -> (WebData BoolResponse -> msg) -> Cmd msg
teamConfirmedGet courseId teamId msg =
    get (unwrap <| teamConfirmed courseId teamId)
        msg
        Bool.decoder


teamUserConfirmedGet : Int -> (WebData BoolResponse -> msg) -> Cmd msg
teamUserConfirmedGet courseId msg =
    get (unwrap <| teamUserConfirmed courseId)
        msg
        Bool.decoder


teamUserConfirmedPut : Int -> (WebData BoolResponse -> msg) -> Cmd msg
teamUserConfirmedPut courseId msg =
    put (unwrap <| teamUserConfirmed courseId)
        Http.emptyBody
        msg
        Bool.decoder


teamJoinPut : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamJoinPut courseId teamId msg =
    put2 (unwrap <| teamJoin courseId)
        (Http.jsonBody <| Encode.object [ ( "team_id", Encode.int teamId ) ])
        msg
        Team.decoder


teamFormPost : Int -> Int -> (WebData Team -> msg) -> Cmd msg
teamFormPost courseId userId msg =
    post (unwrap <| teamForm courseId)
        (Http.jsonBody <| Encode.object [ ( "user_id", Encode.int userId ) ])
        msg
        Team.decoder


teamLeavePut : Int -> (WebData Team -> msg) -> Cmd msg
teamLeavePut courseId msg =
    put2 (unwrap <| teamLeave courseId)
        Http.emptyBody
        msg
        Team.decoder
