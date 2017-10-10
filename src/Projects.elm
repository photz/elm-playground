module Projects exposing (Model, Msg, model, update, view)

import Html exposing (Html, button, div, text, a)
import Html.Attributes exposing (class, href)
import Dict exposing (Dict)

import DataStore
import Router

-- MODEL

type alias Model = { hello : String }

model : Model
model = { hello = "jooohnnyyy" }

-- UPDATE

type Msg = Boom

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = (m, Cmd.none)

-- VIEW

renderItem : DataStore.Model -> DataStore.Project -> Html Msg
renderItem ds p =
    let totalMinutes = DataStore.totalMinutesForProject ds p in
    div
    [ class "project" ]
    [ a [ href (Router.projectPath p.id) ]
          [ p.name |> Html.text ]
    , totalMinutes |> toString |> Html.text
    ]

renderList : DataStore.Model -> Html Msg
renderList ds =
    div []
        (List.map (renderItem ds) (Dict.values ds.projects))

view : DataStore.Model -> Model -> Html Msg
view ds model = div
             [ class "profile" ]
             [ Html.h1 [] [ "Projects" |> Html.text ]
             , renderList ds
             ]
