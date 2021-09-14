module Api.Data.Team exposing
    ( Team
    , TeamBool
    , teamBoolDecoder
    , teamDecoder
    , teamEncoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Team =
    { id : Maybe Int
    , user_id : Int
    , members : List String
    }


type alias TeamBool =
    { bool : Bool }


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" (Decode.nullable Decode.int)
        |> required "user_id" Decode.int
        |> required "members" (Decode.list Decode.string)


teamEncoder : Team -> Encode.Value
teamEncoder model =
    Encode.object
        [ ( "id", maybe Encode.int model.id )
        , ( "use_id", Encode.int model.user_id )
        , ( "members", Encode.list Encode.string model.members )
        ]


teamBoolDecoder : Decoder TeamBool
teamBoolDecoder =
    Decode.succeed TeamBool
        |> required "bool" Decode.bool
