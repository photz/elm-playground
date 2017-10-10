module Server exposing (ServerMsg, decodeMsg, ServerMsg(..))

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Time.Date as Date exposing (Date, date)

import DataStore



type ServerMsg = Projects (List DataStore.Project)
               | Records (List DataStore.Record)
               | Users (List DataStore.User)
               | Unknown

userDecoder : Decode.Decoder DataStore.User
userDecoder = decode DataStore.User
            |> required "id" Decode.int
            |> required "name" Decode.string

projectDecoder : Decode.Decoder DataStore.Project
projectDecoder = decode DataStore.Project
               |> required "id" Decode.int
               |> required "name" Decode.string

dateDecoder : Decode.Decoder Date
dateDecoder = Decode.andThen
              (\dateStr ->
                   case dateStr |> Date.fromISO8601 of
                       Err e -> Decode.fail e
                       Ok d -> Decode.succeed d
              )
              Decode.string


recordDecoder : Decode.Decoder DataStore.Record
recordDecoder = decode DataStore.Record
              |> required "id" Decode.int
              |> required "desc" Decode.string
              |> required "duration" Decode.int
              |> required "project-id" Decode.int
              |> required "date" dateDecoder

multiplexer : String -> Decode.Decoder ServerMsg
multiplexer s =
    case s of
        "projects" ->
            Decode.field "projects" (Decode.map Projects (Decode.list projectDecoder))
        "records" ->
            Decode.field "records" (Decode.map Records (Decode.list recordDecoder))
        "users" ->
            Decode.field "users" (Decode.map Users (Decode.list userDecoder))
        msgType ->
            Decode.fail ("unknown message type" ++ msgType)

serverMsgDecoder : Decode.Decoder ServerMsg
serverMsgDecoder = Decode.field "msg-type" Decode.string
                 |> Decode.andThen multiplexer 

decodeMsg : String -> Result String ServerMsg
decodeMsg s = Decode.decodeString serverMsgDecoder s
