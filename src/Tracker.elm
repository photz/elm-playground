module Tracker exposing (view, Model, Msg, model, update)

import Html exposing (Html, button, div, text, span)
import Html.Attributes exposing (class, classList, style, value)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Time.Date as Date exposing (Date, date)
import DataStore exposing (Record)

-- MODEL

type alias Model = { selectedDate : Date
                   , formProjectId : Maybe Int
                   , formDesc : String
                   , formDuration : Int
                   }

today : Date
today = date 2017 10 3

model : Model
model = { selectedDate = date 2017 10 3
        , formProjectId = Nothing
        , formDuration = 0
        , formDesc = ""
        }

-- UPDATE

type Msg = SelectDate Date
         | FormSelectProject String
         | FormChangeDuration String
         | FormChangeDesc String
         | FormSubmit

update : Msg -> Model -> (Maybe DataStore.Msg, Model)
update msg model =
    case msg of
        SelectDate d ->
            (Nothing, { model | selectedDate = d })
        FormChangeDuration newDuration ->
            (case String.toInt newDuration of
                 Err e -> (Nothing, model)
                 Ok duration ->
                 (Nothing, { model | formDuration = duration })
            )
        FormChangeDesc newDesc ->
            (Nothing, { model | formDesc = newDesc })
        FormSubmit ->
            (case model.formProjectId of
                 Nothing ->
                     (Nothing, model)
                 Just projectId ->
                     let newRecord = { projectId = projectId
                                     , desc = model.formDesc
                                     , duration = model.formDuration
                                     , date = model.selectedDate
                                     }
                     in let msg = DataStore.CreateRecord newRecord
                        in let newModel = { model | formProjectId = Nothing
                                          , formDesc = ""
                                          , formDuration = 0
                                          }
                           in
                        (Just msg, newModel)
            )
        FormSelectProject newId ->
            case String.toInt newId of
                Err e ->
                    (Nothing, model)
                Ok id ->
                    (Nothing, { model | formProjectId = Just id })


-- VIEW

renderDuration : Int -> Html Msg
renderDuration m =
    let h = 60 in
    let label t = span [ class "duration__label" ] [ Html.text t ] in
    let number n = span
                   [ class "duration__number" ]
                   [ n |> toString |> Html.text ] in
    let mLabel = label "m" in
    let hLabel = label "h" in
    span [ class "duration" ]
        (case (m // h, m % h) of
            (0, mins) ->
                [ number mins
                ,  mLabel
                ]
            (hours, 0) ->
                [ number hours
                , hLabel
                ]
            (hours, mins) ->
                [ number hours
                , hLabel
                , number mins
                , mLabel
                ])

renderSelectProject : DataStore.Model -> Html Msg
renderSelectProject m =
    let renderOption (id, p) = Html.option
                               [ id |> toString |> value ]
                               [ p.name |> Html.text ]
    in
    let options = Dict.toList m.projects
                |> List.map renderOption
    in
        Html.select
            [ onInput FormSelectProject
            , class "new-record__project-select"
            ]
            options

renderForm : DataStore.Model -> Model -> Html Msg
renderForm ds m =
    div
        [ class "new-record" ]
        [ renderSelectProject ds
        , Html.textarea
            [ onInput FormChangeDesc
            , m.formDesc |> value
            , class "new-record__desc"
            ]
            []
        , div
              [ class "new-record__bottom" ]
              [ Html.input
                    [ onInput FormChangeDuration
                    , m.formDuration |> toString |> value
                    , class "new-record__duration-input"
                    ]
                    []
              , Html.button
                  [ onClick FormSubmit
                  , class "new-record__save-button"
                  ]
                    [ Html.text "Save" ]

              ]
        ]


renderBar : DataStore.Model -> Model -> Date -> Html Msg
renderBar ds m d
    = 
      let minutes = DataStore.minutesWorked ds d in
      let height = [ minutes |> toString, "px" ] |> String.concat in
      let shadowHeight
              = String.concat [ toString (minutes // 3), "px" ]
      in
      div
      [ onClick (SelectDate d)
      , classList [ ("stats__day", True)
                  , ("stats__day--active", d == m.selectedDate)
                  , ("stats__day--today", d == today)
                  ]
      ]
      [ div [ class "stats__bar-wrapper" ]
            [ div [ classList [ ("stats__bar", True) ]
                  , style [ ("height", height) ]
                  ]
                  []
            , div [ classList [ ("stats__bar", True)
                              , ("stats__bar--shadow", True)
                              ]
                  , style [ ("height", shadowHeight) ]
                  ]
                  []
            ]
      , div
            [ class "stats__date" ]
            [ Date.day d |> toString |> Html.text ]
      ]


renderStats : DataStore.Model -> Model -> Html Msg
renderStats ds m =
    let beginDate = Date.addDays -15 m.selectedDate in

    div [ class "stats" ]

        (List.range 0 30
        |> List.map (\i -> Date.addDays i beginDate)
        |> List.map (\d -> renderBar ds m d))


renderRecord : DataStore.Model ->Record -> Html Msg
renderRecord ds r =
    case r.projectId |> DataStore.getProject ds of
        Nothing -> div [] [ Html.text "invalid project id" ]
        Just project ->
            div
            [ class "record" ]
            [ div [ class "record__project-desc-wrapper" ]
                  [ div [ class "record__project" ]
                        [ project.name |> Html.text ]
                  , div [ class "record__desc" ]
                        [ Html.text r.desc ]
                  ]
            , div [ class "record__duration" ]
                  [ renderDuration r.duration ]
            , div [ class "record__menu" ]
                  [ span
                        [ class "record__button" ]
                        [ "edit" |> Html.text ]
                  , span
                        [ class "record__button" ]
                        [ "remove" |> Html.text ]
                  ]
            ]

renderRecords : DataStore.Model -> Date -> Model -> Html Msg
renderRecords ds d m =
    div
    [ class "records" ]
    (List.map (renderRecord ds) (DataStore.recordsByDay ds d))

returnToTodayBtn : Model -> Html Msg
returnToTodayBtn m =
    if m.selectedDate /= today then
        button
        [ onClick (SelectDate today) ]
        [ "Return to today" |> Html.text ]
    else
        div [] []

renderAddRecordBtn : Html Msg
renderAddRecordBtn
    = button [] [ Html.text "Add a task" ]

container : List (Html a) -> Html a
container xs = div
               [ style [ ("display", "flex")
                       , ("justify-content", "center")
                       ]
               ]
               [ div [ style [ ("max-width", "500px")
                             , ("flex", "auto")
                             ]
                     ]
                     xs
               ]


view : DataStore.Model -> Model -> Html Msg
view ds model = div
             [ class "tracker" ]
             [ renderStats ds model
             , container [ returnToTodayBtn model
                         , Html.h1 [] [ model.selectedDate |> Date.toISO8601 |> Html.text ]
                         , renderForm ds model
                         , renderRecords ds model.selectedDate model
                         , renderAddRecordBtn
                         ]
             ]


