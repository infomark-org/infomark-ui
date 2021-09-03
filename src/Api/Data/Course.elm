module Api.Data.Course exposing (Course, TeamCount, decoder, encoder, teamCountDecoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time exposing (Posix)


type alias Course =
    { id : Int
    , name : String
    , description : String
    , begins_at : Posix
    , ends_at : Posix
    , required_percentage : Int
    , max_team_size : Int
    }

type alias TeamCount =
    { team_count : Int }


decoder : Decoder Course
decoder =
    Decode.succeed Course
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "begins_at" Iso8601.decoder
        |> required "ends_at" Iso8601.decoder
        |> required "required_percentage" Decode.int
        |> required "max_team_size" Decode.int


encoder : Course -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", Encode.string model.name )
        , ( "description", Encode.string model.description )
        , ( "begins_at", Iso8601.encode model.begins_at )
        , ( "ends_at", Iso8601.encode model.ends_at )
        , ( "required_percentage", Encode.int model.required_percentage )
        , ( "max_team_size", Encode.int model.max_team_size )
        ]

teamCountDecoder : Decoder TeamCount
teamCountDecoder =
    Decode.succeed TeamCount
    |> required "team_count" Decode.int
