module Profile exposing (view, Model, Msg, model, update)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)

-- MODEL

type alias Model = { dummy : String }

model : Model
model = { dummy = "foobar" }

-- UPDATE

type Msg = Boom

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = (m, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = div
             [ class "profile" ]
             [ Html.h1 [] [ "profile" |> Html.text ]
             , Html.text model.dummy
             ]
