module DropDown exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events


type alias Settings style msg option =
    { style : style
    , onOptionChosen : Maybe option -> msg
    }


view : Settings style option msg -> List ( option, Element style variant msg ) -> Element style variant msg -> Element style variant msg
view settings options element =
    let
        createClickableElement ( option, optionElement ) =
            Element.el settings.style
                [ Events.onClick (settings.onOptionChosen (Just option)) ]
                optionElement
    in
    element
        |> Element.below
            (List.map createClickableElement options)
