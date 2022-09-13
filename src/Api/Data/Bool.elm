module Api.Data.Bool exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias BoolResponse =
    { bool : Bool
    }


decoder : Decoder BoolResponse
decoder =
    Decode.succeed BoolResponse
        |> required "bool" Decode.bool


encoder : BoolResponse -> Encode.Value
encoder model =
    Encode.object
        [ ( "bool", Encode.bool model.bool )
        ]
