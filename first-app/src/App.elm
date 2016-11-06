module App exposing (..)

import Github
import Errors
import Html exposing (text, div)
import Entries


type alias Model =
    { error : Errors.Model
    , entries : Entries.Model
    }


type Action
    = FetchEventsResultAction Github.Events
    | FetchEventsFailedAction Github.ApiError


users : Entries.Usernames
users =
    [ "torgeir"
    , "emilmork"
    , "mikaelbr"
    ]


initialEntries : Entries.Model
initialEntries =
    []


initialModel : { entries : Entries.Model, error : Errors.Model }
initialModel =
    { entries = initialEntries, error = Nothing }


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.none


init : ( Model, Cmd Action )
init =
    ( initialModel, (Github.fetchAllEvents FetchEventsFailedAction FetchEventsResultAction users) )


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        FetchEventsFailedAction error ->
            let
                errorText =
                    (toString error)
            in
                Debug.log
                    errorText
                    ( { model | error = Just errorText }, Cmd.none )

        FetchEventsResultAction eventList ->
            ( { model | entries = Entries.joinEntries model.entries (List.map eventToEntry eventList) }, Cmd.none )


view : Model -> Html.Html Action
view model =
    div []
        (List.append
            (Errors.view model.error)
            (List.map Entries.view model.entries)
        )


eventToEntry : Github.Event -> Entries.Entry
eventToEntry event =
    { action = event.action
    , user = Entries.Profile event.actor.display_login event.actor.url
    , avatar = event.actor.avatar_url
    , timestamp = event.created_at
    , repo = Entries.Repo event.repo.name event.repo.url
    }
