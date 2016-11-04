port module App exposing (..)

import Json.Decode as Decode exposing ((:=))
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)

type alias Entry =
  { title: String
  , content: String
  }

type alias Model =
  { entries: List Entry
  }

type Msg
  = UpdateEntries (List Entry)
  | Tick Time
  | NoOp

init : (Model, Cmd Msg)
init =
    (defaultState, Cmd.none )

defaultState: Model
defaultState = {
    entries = []
  }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    UpdateEntries entries ->
      ( { model | entries = entries }, Cmd.none )
    Tick time -> (changePositions model, Cmd.none)
    NoOp -> (model, Cmd.none )

changePositions: Model -> Model
changePositions model =
  case model.entries of
    [] ->
      model
    _ ->
      { model | entries = (List.append
        (Maybe.withDefault [] (List.tail model.entries))
        (Maybe.withDefault [] (Maybe.map (\a -> [a]) (List.head model.entries))))
      }

defaultList: List Entry
defaultList =
  []

view : Model -> Html Msg
view model =
    div [ class "spreadsheet-info" ] [
      entryView (List.head model.entries)
     ]

entryView: Maybe Entry -> Html Msg
entryView entry =
  case entry of
    Just entry ->
        div [ class "entries-entry"] [
          h2 [ class "entries-entry-title"] [ text entry.title ],
          h2 [ class "entries-entry-desc"] [ text entry.content]
          ]
    Nothing ->
        h2 [] [text "Empty"]

port entriesport : (Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.batch
        [ entriesport parseEntries
        , Time.every (second * 30) Tick
        ]

parseEntries : Decode.Value -> Msg
parseEntries entriesJson =
  case (decodeModel entriesJson) of
    Ok model -> UpdateEntries model
    _ -> NoOp

decodeModel : Decode.Value -> Result String (List Entry)
decodeModel modelJson =
  Decode.decodeValue entriesDecode modelJson

entriesDecode : Decode.Decoder (List Entry)
entriesDecode =
  Decode.list entryDecode

entryDecode : Decode.Decoder Entry
entryDecode =
  Decode.object2
  Entry
  ("title" := Decode.string)
  ("content" := Decode.string)
