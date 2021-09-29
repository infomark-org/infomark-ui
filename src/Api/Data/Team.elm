module Api.Data.Team exposing
    ( Team
    , TeamBool
    , teamBoolDecoder
    , teamDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Team =
    { id : Maybe Int
    , user_id : Int
    , members : List String
    , member_mails : Maybe (List String)
    }


type alias TeamBool =
    { bool : Bool }


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" (Decode.nullable Decode.int)
        |> required "user_id" Decode.int
        |> required "members" (Decode.list Decode.string)
        |> required "member_mails" (Decode.nullable (Decode.list Decode.string))


teamBoolDecoder : Decoder TeamBool
teamBoolDecoder =
    Decode.succeed TeamBool
        |> required "bool" Decode.bool
