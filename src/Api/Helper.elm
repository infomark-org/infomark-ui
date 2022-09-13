module Api.Helper exposing
    ( delete
    , deleteExpectNothing
    , get
    , patch
    , patchExpectNothing
    , post
    , postExpectNothing
    , postFile
    , put
    , put2
    , putExpectNothing
    )

import File exposing (File)
import Http exposing (..)
import Json.Decode exposing (Decoder, decodeString, errorToString)
import RemoteData exposing (RemoteData(..), WebData)


get : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get url msg decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
post url body msg decoder =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postExpectNothing : String -> Http.Body -> (WebData () -> msg) -> Cmd msg
postExpectNothing url body msg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Uploads a single file. You can subscribe to the tracker "file\_upload" using

    type Msg
        = GotProgress Http.Progress

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Http.track "file_upload" GotProgress

-}
postFile : String -> File -> (WebData () -> msg) -> Cmd msg
postFile url file msg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.multipartBody [ Http.filePart "file_data" file ]
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Just "file_upload"
        }


patch : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
patch url body msg decoder =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


patchExpectNothing : String -> Http.Body -> (WebData () -> msg) -> Cmd msg
patchExpectNothing url body msg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


put : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
put url body msg decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


put2 : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
put2 url body msg decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = expectJson2 (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectJson2 : (Result Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson2 toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    -- Err (Http.BadStatus metadata.statusCode)
                    Err (Http.BadBody body)

                Http.GoodStatus_ metadata body ->
                    case decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (errorToString err))


putExpectNothing : String -> Http.Body -> (WebData () -> msg) -> Cmd msg
putExpectNothing url body msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
delete url msg decoder =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteExpectNothing : String -> (WebData () -> msg) -> Cmd msg
deleteExpectNothing url msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }
