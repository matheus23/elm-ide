module Util exposing (..)

import Html exposing (Html)


tightHtml : (msg -> model -> model) -> (model -> Html msg) -> model -> Html model
tightHtml update view model =
    let
        applyUpdate msg =
            update msg model
    in
    Html.map applyUpdate (view model)
