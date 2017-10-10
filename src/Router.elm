module Router exposing (Msg, Model, Route(..), parseLocation, projectPath)

import Navigation
import UrlParser exposing ((</>))
import DataStore

-- MODEL



matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map TrackerRoute UrlParser.top
        , UrlParser.map ProfileRoute
            (UrlParser.s "profile")
        , UrlParser.map ProjectsRoute
            (UrlParser.s "project")
        , UrlParser.map TrackerRoute
            (UrlParser.s "tracker")
        , UrlParser.map UsersRoute
            (UrlParser.s "users")
        , UrlParser.map ProjectRoute
            (UrlParser.s "projects" </> UrlParser.int)
        ]

type Route = ProfileRoute
           | TrackerRoute
           | ProjectsRoute
           | NotFoundRoute
           | UsersRoute
           | ProjectRoute DataStore.ProjectId

type alias Model = Route


parseLocation : Navigation.Location -> Route
parseLocation l =
    case UrlParser.parseHash matchers l of
        Just route ->
            route

        Nothing ->
            NotFoundRoute

-- UPDATE

type Msg = GoTo Route


projectPath : DataStore.ProjectId -> String
projectPath pid = "#projects/" ++ (pid |> toString)
