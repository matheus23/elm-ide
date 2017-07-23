module Util exposing (..)

import Element exposing (Element)
import Html exposing (Html)


tightHtml : (msg -> model -> model) -> (model -> Html msg) -> model -> Html model
tightHtml update view model =
    let
        applyUpdate msg =
            update msg model
    in
    Html.map applyUpdate (view model)


tightElement :
    (msg -> model -> model)
    -> (model -> Element style variation msg)
    -> model
    -> Element style variation model
tightElement update view model =
    let
        applyUpdate msg =
            update msg model
    in
    Element.map applyUpdate (view model)
