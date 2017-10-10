import Html exposing (Html, button, div, li, a)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import Debug
import Mouse
import WebSocket

import Users
import Server
import Router
import Tracker
import Profile
import DataStore
import Projects
import Project

server : String
server = "ws://127.0.0.1:3000"

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.clicks MouseMsg
              , WebSocket.listen server WsMsg
              ]

main = Navigation.program OnLocationChange
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

type alias Model = { tracker : Tracker.Model
                   , profile : Profile.Model
                   , projects : Projects.Model
                   , dataStore : DataStore.Model
                   , page : Router.Route
                   }

initialModel : Model
initialModel = { tracker = Tracker.model
               , profile = Profile.model
               , projects = Projects.model
               , dataStore = DataStore.model
               , page = Router.TrackerRoute
               }

getProjects : Cmd Msg
getProjects = WebSocket.send server "{\"type\":\"get-projects\"}"

getRecords : Cmd Msg
getRecords = WebSocket.send server "{\"type\":\"get-records\"}"

getUsers : Cmd Msg
getUsers = WebSocket.send server "{\"type\":\"get-users\"}"

init : Navigation.Location -> ( Model, Cmd Msg )
init l =
    let route = Router.parseLocation l in
    let model = { initialModel | page = route } in
    ( initialModel, Cmd.batch [ getProjects
                              , getRecords
                              , getUsers
                              ])

pages : List (String, String)
pages = [ ("Profile", "#profile")
        , ("Tracker", "#tracker")
        , ("Projects", "#project")
        , ("Users", "#users")
        ]

-- UPDATE

type Msg = TrackerMsg Tracker.Msg
         | ProfileMsg Profile.Msg
         | ProjectsMsg Projects.Msg
         | MouseMsg Mouse.Position
         | OnLocationChange Navigation.Location
         | WsMsg String
         | UsersMsg Users.Msg
         | ProjectMsg Project.Msg


updateWs : Model -> String -> ( Model, Cmd Msg )
updateWs m s =
    let res = Server.decodeMsg s in
    case res of
        Err e ->
            let _ = Debug.log e s in
            ( m, Cmd.none )
        Ok serverMsg -> 
            let x = Debug.log "msg" serverMsg in
            case serverMsg of 
                Server.Projects p ->
                    let newDs = DataStore.addProjects m.dataStore p in
                    ( { m | dataStore = newDs }, Cmd.none )
                Server.Records r ->
                    let newDs = DataStore.addRecords m.dataStore r in
                    ( { m | dataStore = newDs }, Cmd.none )
                Server.Users u -> 
                    let newDs = DataStore.addUsers m.dataStore u in
                    ( { m | dataStore = newDs }, Cmd.none )
                _ ->
                    ( m, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
         WsMsg wsMessageAsStr -> updateWs model wsMessageAsStr

         MouseMsg pos ->
            (model, Cmd.none)

         OnLocationChange l ->
            let route = Router.parseLocation l in
            ({ model | page = route }, Cmd.none)

         TrackerMsg trackerMsg ->
             let (m, c) = Tracker.update trackerMsg model.tracker in
             ({ model | tracker = m }, c |> Cmd.map TrackerMsg)

         ProjectMsg projectMsg ->
             ( model, Cmd.none )

         ProjectsMsg projectMsg ->
             let (m, c) = Projects.update projectMsg model.projects in
             ({ model | projects = m }, c |> Cmd.map ProjectsMsg)

         ProfileMsg profileMsg ->
             let (m, c) = Profile.update profileMsg model.profile in
             ({ model | profile = m }, c |> Cmd.map ProfileMsg)

         UsersMsg usersMsg ->
             (model, Cmd.none)
             

-- VIEW


navItem : String -> String -> Bool -> Html Msg
navItem name url active =
    li
    [ classList [ ("nav__item", True)
                , ("nav__item--active", active)
                ]
    ]
    [ a
      [ href url
      , class "nav__link"
      ]
      [ Html.text name ]
    ]


nav : Model -> Html Msg
nav m = 
    div
    [ class "nav" ]
    (List.map (\(name, url) -> navItem name url False) pages)


page : Model -> Html Msg
page model =
    case model.page of
        Router.ProjectRoute projectId ->
            Project.view model.dataStore projectId |> Html.map ProjectMsg
        Router.ProfileRoute ->
            Profile.view model.profile |> Html.map ProfileMsg
        Router.TrackerRoute ->
            Tracker.view model.dataStore model.tracker |> Html.map TrackerMsg
        Router.ProjectsRoute ->
            Projects.view model.dataStore model.projects |> Html.map ProjectsMsg
        Router.NotFoundRoute ->
            div [] [ "404" |> Html.text ]

        Router.UsersRoute ->
            Users.view model.dataStore |> Html.map UsersMsg
        


view : Model -> Html Msg
view model = div
             [ class "app" ]
             [ nav model
             , div [ class "content" ] [ page model ]
             ]
                 
