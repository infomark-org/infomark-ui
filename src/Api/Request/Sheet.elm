module Api.Request.Sheet exposing
    ( sheetDelete
    , sheetFilePost
    , sheetGet
    , sheetPointsGet
    , sheetPut
    , sheetTasksGet
    , sheetTasksPost
    )

import Api.Data.Grade as Grade exposing (Grade)
import Api.Data.PointOverview as PointOverview exposing (PointOverview)
import Api.Data.Sheet as Sheet exposing (Sheet)
import Api.Data.Task as Task exposing (Task)
import Api.Endpoint exposing (sheet, sheetFile, sheetPoints, sheetTasks, unwrap)
import Api.Helper exposing (deleteExpectNothing, get, post, postFile, putExpectNothing)
import File exposing (File)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


sheetGet : Int -> Int -> (WebData Sheet -> msg) -> Cmd msg
sheetGet courseId id msg =
    get (unwrap <| sheet courseId id) msg Sheet.decoder


sheetPut : Int -> Int -> Sheet -> (WebData () -> msg) -> Cmd msg
sheetPut courseId id sheetUpdate msg =
    putExpectNothing (unwrap <| sheet courseId id)
        (Http.jsonBody <| Sheet.encoder sheetUpdate)
        msg


sheetDelete : Int -> Int -> (WebData () -> msg) -> Cmd msg
sheetDelete courseId id msg =
    deleteExpectNothing (unwrap <| sheet courseId id) msg


sheetFilePost : Int -> Int -> File -> (WebData () -> msg) -> Cmd msg
sheetFilePost courseId id file msg =
    postFile (unwrap <| sheetFile courseId id) file msg


sheetTasksPost : Int -> Int -> Task -> (WebData Task -> msg) -> Cmd msg
sheetTasksPost courseId id taskNew msg =
    post (unwrap <| sheetTasks courseId id)
        (Http.jsonBody <| Task.encoder taskNew)
        msg
        Task.decoder


sheetTasksGet : Int -> Int -> (WebData (List Task) -> msg) -> Cmd msg
sheetTasksGet courseId id msg =
    get (unwrap <| sheetTasks courseId id)
        msg
    <|
        Decode.list Task.decoder


sheetPointsGet : Int -> Int -> (WebData (List PointOverview) -> msg) -> Cmd msg
sheetPointsGet courseId sheetId msg =
    get (unwrap <| sheetPoints courseId sheetId)
        msg
    <|
        Decode.list PointOverview.decoder
