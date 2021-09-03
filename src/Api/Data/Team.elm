module Api.Data.Team exposing
    ( Team
    , TeamMember
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
    , members : List TeamMember
    }


type alias TeamMember =
    { first_name : String
    , last_name : String
    }


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" (Decode.nullable Decode.int)
        |> required "user_id" Decode.int
        |> required "members" (Decode.list teamMemberDecoder)


teamMemberDecoder : Decoder TeamMember
teamMemberDecoder =
    Decode.succeed TeamMember
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string


teamEncoder : Team -> Encode.Value
teamEncoder model =
    Encode.object
        [ ( "id", maybe Encode.int model.id )
        , ( "use_id", Encode.int model.user_id )
        , ( "members", Encode.list teamMemberEncoder model.members )
        ]


teamMemberEncoder : TeamMember -> Encode.Value
teamMemberEncoder model =
    Encode.object
        [ ( "first_name", Encode.string model.first_name )
        , ( "last_name", Encode.string model.last_name )
        ]

