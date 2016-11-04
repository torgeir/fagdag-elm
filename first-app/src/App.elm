module App exposing (..)

import Html exposing (text, div)


type alias Entry =
    { user : String
    , action : String
    }


type alias Entries =
    List Entry


initialEntries =
    [ { user = "@torgeir", action = "forked <repo> to <forked-repo>" }
    , { user = "@emilmork", action = "starred <repo>" }
    ]


entryView : Entry -> Html.Html Msg
entryView entry =
    div []
        [ text entry.user
        , text " "
        , text entry.action
        ]


type alias Model =
    Entries


type Msg
    = String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialEntries, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        (List.map entryView model)
