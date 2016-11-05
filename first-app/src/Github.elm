module Github
    exposing
        ( fetchAllEvents
        , ApiError
        , Username
        , Events
        , Event
        , EventActor
        )

import Http exposing (..)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)


fetchAllEvents failedMsg successMsg usernames =
    let
        cmds =
            List.map (fetchUserEvents failedMsg successMsg) usernames
    in
        Cmd.batch cmds


fetchUserEvents failedMsg successMsg user =
    let
        url =
            "https://api.github.com/users/" ++ user ++ "/events/public"

        request =
            (Http.get decoderEvents url)
    in
        (Task.perform failedMsg successMsg request)


type alias ApiError =
    Http.Error


type alias Username =
    String


type alias Events =
    List Event


type alias Event =
    { id : String
    , eventType : String
    , created_at : String
    , actor : EventActor
    }


type alias EventActor =
    { display_login : String }


decoderEvents : Json.Decoder (List Event)
decoderEvents =
    Json.list decoderEvent


decoderEvent : Json.Decoder Event
decoderEvent =
    Json.object4
        Event
        ("id" := Json.string)
        ("type" := Json.string)
        ("created_at" := Json.string)
        ("actor" := decoderEventActor)


decoderEventActor : Json.Decoder EventActor
decoderEventActor =
    Json.object1
        EventActor
        ("display_login" := Json.string)
