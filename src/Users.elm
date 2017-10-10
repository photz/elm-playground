module Users exposing (view, Msg)

import Html exposing (Html, div, h1)
import Html.Attributes exposing (class)
import Dict
import DataStore

-- UPDATE

type Msg = Foo

-- VIEW

renderUser : DataStore.User -> Html Msg
renderUser u = div [] [ Html.text u.name ]

view : DataStore.Model -> Html Msg
view ds = div []
          [ h1 [] [ Html.text "Users" ]
          , div [] (List.map renderUser (Dict.values ds.users))
          ]
