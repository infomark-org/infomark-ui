module Api.Data.Team exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias Team =
    { id : Maybe Int
    , user_id : Int
    , members : List String
    , member_mails : List String
    }


decoder : Decoder Team
decoder =
    Decode.succeed Team
        |> optional "id" (Decode.nullable Decode.int) Nothing
        |> required "user_id" Decode.int
        |> required "members" (Decode.list Decode.string)
        |> optional "member_mails" (Decode.list Decode.string) []


encoder : Team -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", maybe Encode.int model.id )
        , ( "user_id", Encode.int model.user_id )
        , ( "members", Encode.list Encode.string model.members )
        , ( "member_mails", Encode.list Encode.string model.member_mails )
        ]
