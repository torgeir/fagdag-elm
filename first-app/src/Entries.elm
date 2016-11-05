module Entries exposing (..)

import Html exposing (div, text)


type alias Model =
    List Entry


type alias Entry =
    { user : User
    , action : String
    , timestamp : String
    }


type alias User =
    String


type alias Users =
    List User


view : Entry -> Html.Html a
view entry =
    div []
        [ text "@"
        , text entry.user
        , text " "
        , text entry.timestamp
        , text " "
        , text entry.action
        ]


joinEntries : Model -> Model -> Model
joinEntries entries moreEntries =
    entries
        |> List.append moreEntries
        |> List.sortBy .timestamp
        |> List.reverse
