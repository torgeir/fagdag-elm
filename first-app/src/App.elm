module App exposing (..)

import Html exposing (text, div)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Http exposing (..)


type alias EventActor =
    { display_login : String }


type alias Event =
    { id : String
    , eventType : String
    , created_at : String
    , actor : EventActor
    }


type alias Entry =
    { user : User
    , action : String
    , timestamp : String
    }


type alias Entries =
    List Entry


type alias User =
    String


type alias Users =
    List User


users =
    [ "torgeir", "emilmork" ]


initialEntries =
    []


entryView : Entry -> Html.Html Msg
entryView entry =
    div []
        [ text "@"
        , text entry.user
        , text " "
        , text entry.timestamp
        , text " "
        , text entry.action
        ]


type alias Model =
    Entries


type Msg
    = String
    | OkMsg (List Event)
    | FailedMsg Http.Error


decoder : Json.Decoder (List Event)
decoder =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


eventToEntry : Event -> Entry
eventToEntry event =
    { action = event.eventType
    , user = event.actor.display_login
    , timestamp = event.created_at
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OkMsg eventList ->
            ( (List.map eventToEntry eventList), Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialEntries, (Task.perform FailedMsg OkMsg (Http.get decoder "https://api.github.com/users/torgeir/events/public")) )


view : Model -> Html.Html Msg
view model =
    div []
        (List.map entryView model)
