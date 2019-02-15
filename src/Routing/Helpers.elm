module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = LoginRoute
    | RegistrationRoute
    | DashboardRoute
    | CoursesRoute
    | CreateCourseRoute
    | EditCourseRoute Int
    | CourseDetailRoute Int
    | CreateSheetRoute
    | EditSheetRoute Int
    | SheetDetailRoute Int
    | CreateTaskRoute
    | EditTaskRoute Int
    | SubmissionGradingRoute Int Int
    | AdminRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    let
        pieces =
            case route of
                LoginRoute ->
                    []

                RegistrationRoute ->
                    [ "registration" ]

                DashboardRoute ->
                    [ "dashboard" ]

                CoursesRoute ->
                    [ "courses" ]

                CreateCourseRoute ->
                    [ "course", "create" ]

                EditCourseRoute id ->
                    [ "course", String.fromInt id, "edit" ]

                CourseDetailRoute id ->
                    [ "course", String.fromInt id ]

                CreateSheetRoute ->
                    [ "sheet", "create" ]

                EditSheetRoute id ->
                    [ "sheet", String.fromInt id, "edit" ]

                SheetDetailRoute id ->
                    [ "sheet", String.fromInt id ]

                CreateTaskRoute ->
                    [ "task", "create" ]

                EditTaskRoute id ->
                    [ "task", String.fromInt id, "edit" ]

                SubmissionGradingRoute taskId groupId ->
                    [ "task", String.fromInt taskId, "grade", "group", String.fromInt groupId]

                AdminRoute ->
                    [ "admin" ]

                _ ->
                    []
    in
    "#/" ++ String.join "/" pieces


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map LoginRoute Url.Parser.top
        , Url.Parser.map RegistrationRoute (Url.Parser.s "registration")
        , Url.Parser.map DashboardRoute (Url.Parser.s "dashboard")
        , Url.Parser.map CoursesRoute (Url.Parser.s "courses")
        , Url.Parser.map CourseDetailRoute (Url.Parser.s "course" </> Url.Parser.int)
        , Url.Parser.map CreateCourseRoute (Url.Parser.s "course" </> Url.Parser.s "create")
        , Url.Parser.map EditCourseRoute (Url.Parser.s "course" </> Url.Parser.int </> Url.Parser.s "edit")
        , Url.Parser.map SheetDetailRoute (Url.Parser.s "sheet" </> Url.Parser.int)
        , Url.Parser.map CreateSheetRoute (Url.Parser.s "sheet" </> Url.Parser.s "create")
        , Url.Parser.map EditSheetRoute (Url.Parser.s "sheet" </> Url.Parser.int </> Url.Parser.s "edit")
        , Url.Parser.map CreateTaskRoute (Url.Parser.s "task" </> Url.Parser.s "create")
        , Url.Parser.map EditTaskRoute (Url.Parser.s "task" </> Url.Parser.int </> Url.Parser.s "edit")
        , Url.Parser.map SubmissionGradingRoute (Url.Parser.s "task" </> Url.Parser.int </> Url.Parser.s "grade" </> Url.Parser.s "group" </> Url.Parser.int)
        , Url.Parser.map AdminRoute (Url.Parser.s "admin")
        ]


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            LoginRoute

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFoundRoute