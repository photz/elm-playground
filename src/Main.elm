import Html exposing (Html, button, div, li)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)

import Tracker
import Profile
import DataStore
import Project

main = Html.beginnerProgram { model = model
                            , view = view
                            , update = update
                            }

type alias Model = { tracker : Tracker.Model
                   , profile : Profile.Model
                   , project : Project.Model
                   , dataStore : DataStore.Model
                   , page : Page
                   }

model : Model
model = { tracker = Tracker.model
        , profile = Profile.model
        , project = Project.model
        , dataStore = DataStore.model
        , page = Tracker
        }

pages : List (Page, String)
pages = [ (Profile, "Profile")
        , (Tracker, "Tracker")
        , (Project, "Projects")
        ]

type Page = Profile
          | Tracker
          | Project


-- UPDATE

type Msg = TrackerMsg Tracker.Msg
         | ProfileMsg Profile.Msg
         | ProjectMsg Project.Msg
         | NavMsg Page

update : Msg -> Model -> Model
update msg model =
    case msg of
        TrackerMsg trackerMsg ->
            let (maybeDataStoreMsg, newModel) = Tracker.update trackerMsg model.tracker
            in
                (case maybeDataStoreMsg of 
                    Nothing ->
                        { model | tracker = newModel }
                    Just dataStoreMsg -> 
                        { model | tracker = newModel
                        , dataStore = DataStore.update dataStoreMsg model.dataStore })
        ProjectMsg projectMsg ->
            { model | project = Project.update projectMsg model.project }
        ProfileMsg profileMsg ->
            { model | profile = Profile.update profileMsg model.profile }
        NavMsg page ->
            { model | page = page }


-- VIEW


navItem : Page -> String -> Bool -> Html Msg
navItem p s b =
    li
    [ onClick (NavMsg p)
    , classList [ ("nav__item", True)
                , ("nav__item--active", b)
                ]
    ]
    [ Html.text s ]

nav : Model -> Html Msg
nav m = 
    div
    [ class "nav" ]
    (List.map (\(p, s) -> navItem p s (p == m.page)) pages)

page : Model -> Html Msg
page model =
    case model.page of
        Profile ->
            Profile.view model.profile |> Html.map ProfileMsg
        Tracker ->
            Tracker.view model.dataStore model.tracker |> Html.map TrackerMsg
        Project ->
            Project.view model.dataStore model.project |> Html.map ProjectMsg

view : Model -> Html Msg
view model = div
             [ class "app" ]
             [ nav model
             , div [ class "content" ] [ page model ]
             ]
                 
