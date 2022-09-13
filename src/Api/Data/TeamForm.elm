module Api.Data.TeamForm exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias TeamForm =
    { user_id : Int
    }


decoder : Decoder TeamForm
decoder =
    Decode.succeed TeamForm
        |> required "user_id" Decode.int


encoder : TeamForm -> Encode.Value
encoder model =
    Encode.object
        [ ( "user_id", Encode.int model.user_id )
        ]
