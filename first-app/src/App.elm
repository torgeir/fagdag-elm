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
    [ "torgeir"
    , "emilmork"
    ]


initialEntries =
    []


initialModel =
    { error = Nothing, entries = initialEntries }


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


type alias ErrorMessage =
    Maybe String


type alias Model =
    { error : ErrorMessage
    , entries : Entries
    }


type Msg
    = String
    | FetchEventsResultMsg (List Event)
    | FetchEventsFailedMsg Http.Error


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


joinEntries : Entries -> Entries -> Entries
joinEntries entries moreEntries =
    entries
        |> List.append moreEntries
        |> List.sortBy .timestamp
        |> List.reverse


errorView : ErrorMessage -> List (Html.Html Msg)
errorView error =
    Maybe.withDefault []
        (Maybe.map
            (\errorText -> [ div [] [ text errorText ] ])
            error
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchEventsFailedMsg error ->
            let
                errorText =
                    (toString error)
            in
                Debug.log errorText
                    ( { model | error = Just errorText }, Cmd.none )

        FetchEventsResultMsg eventList ->
            ( { model | entries = joinEntries model.entries (List.map eventToEntry eventList) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchAllEvents )


view : Model -> Html.Html Msg
view model =
    div []
        (List.append
            (errorView model.error)
            (List.map entryView model.entries)
        )


fetchAllEvents : Cmd Msg
fetchAllEvents =
    let
        cmds =
            List.map fetchUserEvents users
    in
        Cmd.batch cmds


fetchUserEvents : String -> Cmd Msg
fetchUserEvents user =
    let
        url =
            "https://api.github.com/users/" ++ user ++ "/events/public"

        request =
            (Http.get decoder url)
    in
        (Task.perform FetchEventsFailedMsg FetchEventsResultMsg request)
