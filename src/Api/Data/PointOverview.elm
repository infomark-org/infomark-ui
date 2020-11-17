module Api.Data.PointOverview exposing (PointOverview, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias PointOverview =
    { acquired_points : Int
    , graded : Bool
    , max_points : Int
    , sheet_id : Maybe Int
    , sheet_graded : Maybe Int
    , task_id : Maybe Int
    }


decoder : Decoder PointOverview
decoder =
    Decode.succeed PointOverview
        |> required "acquired_points" Decode.int
        |> required "graded" Decode.bool
        |> required "max_points" Decode.int
        |> optional "sheet_id" (Decode.nullable Decode.int) Nothing
        |> optional "sheet_graded" (Decode.nullable Decode.int) Nothing
        |> optional "task_id" (Decode.nullable Decode.int) Nothing


encoder : PointOverview -> Encode.Value
encoder model =
    Encode.object
        [ ( "acquired_points", Encode.int model.acquired_points )
        , ( "graded", Encode.bool model.graded )
        , ( "max_points", Encode.int model.max_points )
        , case ( model.sheet_graded ) of
            ( Just g ) -> ( "sheet_graded", Encode.int g )
            _          -> ( "", Encode.null )
        , case ( model.sheet_id, model.task_id ) of
            ( Just id, _ ) ->
                ( "sheet_id", Encode.int id )

            ( _, Just id ) ->
                ( "task_id", Encode.int id )

            ( _, _ ) ->
                ( "", Encode.null )
        ]
