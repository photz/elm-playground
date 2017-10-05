module DataStore exposing (..)

import Time.Date as Date exposing (Date, date)
import Dict exposing (Dict)

-- MODEL

type alias RecordId = Int

type alias ProjectId = Int

type alias Project = { id : ProjectId
                     , name : String
                     }

type alias Record = { id : RecordId
                    , desc : String
                    , duration : Int
                    , projectId : ProjectId
                    }

type alias Model = { days : Dict (Int, Int, Int) (List RecordId)
                   , records : Dict RecordId Record
                   , projects : Dict ProjectId Project
                   }

totalMinutesForProject : Model -> Project -> Int
totalMinutesForProject m p =
    m.records
        |> Dict.values
        |> List.filter (\r -> r.projectId == p.id)
        |> List.foldr (\r acc -> r.duration + acc) 0


recordsByDay : Model -> Date -> List Record
recordsByDay m d
    = case Dict.get (Date.toTuple d) m.days of
          Nothing -> []
          Just ids ->
              List.map (\id -> Dict.get id m.records) ids
                  |> List.filterMap identity
      
dummyDays : Dict (Int, Int, Int) (List RecordId)
dummyDays = Dict.fromList
            [ ((2017, 10, 3), [ 1, 2 ])
            , ((2017, 10, 1), [ 3, 4, 5 ])
            ]

dummyProjects : Dict ProjectId Project
dummyProjects = Dict.fromList
                [ (1, { id = 1
                      , name = "project A"
                      }
                  )
                , (2, { id = 2
                      , name = "project B"
                      }
                  )
                , (3, { id = 3
                      , name = "project C"
                      }
                  )
                ]


dummyRecords : Dict RecordId Record
dummyRecords = Dict.fromList
               [ (1, { id = 1
                     , desc = "with id 1"
                     , duration = 90
                     , projectId = 1})
               , (2, { id = 2
                     , desc = "with id 2"
                     , duration = 15
                     , projectId = 2})
               , (3, { id = 3
                     , desc = "with id 3"
                     , duration = 30
                     , projectId = 2})
               , (4, { id = 4
                     , desc = "with id 4"
                     , duration = 30
                     , projectId = 2 })
               , (5, { id = 5
                     , desc = "with id 5"
                     , duration = 90
                     , projectId = 1 })
               ]

minutesWorked : Model -> Date -> Int
minutesWorked m d
    = recordsByDay m d
    |> List.foldr (\ record acc -> record.duration + acc) 0

model : Model
model = { days = dummyDays
        , records = dummyRecords
        , projects = dummyProjects
        }

getProject : Model -> ProjectId -> Maybe Project
getProject m id = Dict.get id m.projects 

-- UPDATE

type Msg = CreateRecord { projectId : ProjectId
                        , desc : String
                        , duration : Int
                        , date : Date
                        }

insertRecord : Date -> Record -> Model -> Model
insertRecord d r m =
    let nextId = 1000 in
    let records = Dict.insert nextId r m.records in
    let days = Dict.update (Date.toTuple d) (\maybeIds -> case maybeIds of
                          Nothing -> Just [ nextId ]
                          Just ids -> Just (nextId :: ids)) m.days

    in 
        { m | records = records, days = days }
                 

update : Msg -> Model -> Model
update m mod =
    case m of
        CreateRecord { desc, projectId, duration, date } ->
            let newRecord = { id = 0
                            , desc = desc
                            , duration = duration
                            , projectId = projectId
                            }
            in
                insertRecord date newRecord mod
