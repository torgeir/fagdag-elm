module Errors exposing (..)

import Html exposing (text, div)


type alias Model =
    Maybe String


view : Model -> List (Html.Html a)
view error =
    Maybe.withDefault []
        (Maybe.map
            (\errorText -> [ div [] [ text errorText ] ])
            error
        )
