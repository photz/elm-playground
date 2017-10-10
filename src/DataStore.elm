module DataStore exposing (..)

import Time.Date as Date exposing (Date, date)
import Dict exposing (Dict)
import WebSocket
import Debug
import Dict.Extra as Dict exposing (groupBy)

-- MODEL

type alias UserId = Int

type alias RecordId = Int

type alias ProjectId = Int

type alias Project = { id : ProjectId
                     , name : String
                     }

type alias User = { id : UserId
                  , name : String
                  }

type alias Record = { id : RecordId
                    , desc : String
                    , duration : Int
                    , projectId : ProjectId
                    , date : Date
                    }

type alias Model = { days : Dict (Int, Int, Int) (List RecordId)
                   , records : Dict RecordId Record
                   , projects : Dict ProjectId Project
                   , users : Dict UserId User
                   }

totalMinutesForProject : Model -> Project -> Int
totalMinutesForProject m p =
    m.records
        |> Dict.values
        |> List.filter (\r -> r.projectId == p.id)
        |> List.foldr (\r acc -> r.duration + acc) 0

recordsForProject : Model -> ProjectId -> List Record
recordsForProject m pid =
    Dict.values m.records
        |> List.filter (.projectId >> ((==) pid))

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
                     , date = Date.date 2017 10 3
                     , duration = 90
                     , projectId = 1})
               , (2, { id = 2
                     , desc = "with id 2"
                     , date = Date.date 2017 10 3
                     , duration = 15
                     , projectId = 2})
               , (3, { id = 3
                     , desc = "with id 3"
                     , date = Date.date 2017 10 3
                     , duration = 30
                     , projectId = 2})
               , (4, { id = 4
                     , desc = "with id 4"
                     , date = Date.date 2017 10 3
                     , duration = 30
                     , projectId = 2 })
               , (5, { id = 5
                     , desc = "with id 5"
                     , date = Date.date 2017 10 3
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
        , users = Dict.empty
        }

getProject : Model -> ProjectId -> Maybe Project
getProject m id = Dict.get id m.projects 

-- UPDATE

type Msg = CreateRecord Record

addRecords : Model -> List Record -> Model
addRecords m r =
    let records = List.foldr (\r d -> Dict.insert r.id r d) Dict.empty r in

    let datesIds = List.map (\r -> (Date.toTuple r.date, r.id)) r in

            
    let datesToRecs = Dict.groupBy (\r -> Date.toTuple r.date) r in

    
    let recsToIds recs = List.map .id recs in

    let x = Dict.map (always recsToIds) datesToRecs in

    { m | records = records, days = x }

addProjects : Model -> List Project -> Model
addProjects m p =
    let newProjects =
            List.foldr (\p d -> Dict.insert p.id p d)
                Dict.empty
                p
    in
        { m | projects = newProjects }

addUsers : Model -> List User -> Model
addUsers m us =
    let users = List.foldr
                (\u d -> Dict.insert u.id u d)
                Dict.empty
                us
    in
        { m | users = users }

insertRecord : Date -> Record -> Model -> Model
insertRecord d r m =
    let nextId = 1000 in
    let records = Dict.insert nextId r m.records in
    let days = Dict.update (Date.toTuple d) (\maybeIds -> case maybeIds of
                          Nothing -> Just [ nextId ]
                          Just ids -> Just (nextId :: ids)) m.days

    in 
        { m | records = records, days = days }
                 
serverUrl : String
serverUrl = "ws://echo.websocket.org"


update : Msg -> Model -> (Model, Cmd Msg)
update m mod =
    case m of
        CreateRecord { desc, projectId, duration, date } ->
            let newRecord = { id = 0
                            , desc = desc
                            , duration = duration
                            , projectId = projectId
                            , date = date
                            }
            in
                let s = WebSocket.send serverUrl "test" in
                let p = Debug.log "hello" "boom" in
                (insertRecord date newRecord mod, s)
