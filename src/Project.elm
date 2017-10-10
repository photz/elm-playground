module Project exposing (view, Msg)

import Html exposing (Html, div)
import DataStore
import Dict exposing (Dict)

type Msg = Foo

renderRecord : DataStore.Record -> Html Msg
renderRecord r =
    div [] [ Html.text r.desc ]

view : DataStore.Model -> DataStore.ProjectId -> Html Msg
view m pid = 
    case Dict.get pid m.projects of
        Nothing ->
            div [] [ "Project not found" |> Html.text ]
        Just project ->
            div [] [ Html.text project.name,
                     div []
                         (DataStore.recordsForProject m pid
                         |> List.map renderRecord)
                   ]

