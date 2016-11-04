port module App exposing (..)

import Json.Decode as Decode exposing ((:=))
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Entry =
  { title: String
  , content: String
  }

type alias Model =
  { entries: List Entry
  }

type Msg
  = UpdateEntries (List Entry)
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    UpdateEntries entries ->
      ( { model | entries = entries }, Cmd.none )
    NoOp -> (model, Cmd.none )

init : (Model, Cmd Msg)
init =
    (defaultState, Cmd.none )

defaultState: Model
defaultState = {
    entries = []
  }

view : Model -> Html Msg
view model =
    div [] [
      entriesView model.entries
     ]

entriesView: List Entry -> Html Msg
entriesView entries =
  div [] (List.map entryView entries)

entryView: Entry -> Html Msg
entryView { title, content } =
  div [] [
    h2 [] [ text title ],
    h2 [] [ text content]
  ]

port entriesport : (Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  entriesport parseEntries

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
