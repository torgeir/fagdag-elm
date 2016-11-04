module App exposing (..)

import Html exposing (text, div)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Http exposing (..)


type alias Event =
    { id : String }


type alias Entry =
    { id : String
    , user : User
    , action : String
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
    [ { id = "n-a", user = "torgeir", action = "forked <repo> to <forked-repo>" }
    , { id = "n-a", user = "emilmork", action = "starred <repo>" }
    ]


entryView : Entry -> Html.Html Msg
entryView entry =
    div []
        [ text entry.id
        , text entry.user
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
    Json.object1
        Event
        ("id" := Json.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


eventToEntry : Event -> Entry
eventToEntry event =
    { id = event.id
    , action = ""
    , user = ""
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
