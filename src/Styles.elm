module Styles exposing (..)

import Color
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type alias Variations =
    Never


type Styles
    = NoStyle
    | Keyword
    | Identifier
    | Dragged
    | DragHover
    | Droppable


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style NoStyle
            []
        , style Keyword
            [ Font.typeface [ "Lucida Console" ]
            , Color.text Color.lightBlue
            ]
        , style Identifier [ Font.typeface [ "Lucida Console" ] ]
        , style Dragged
            [ Color.text Color.grey
            ]
        , style DragHover
            [ Border.all 1
            ]
        , style Droppable
            [ Border.all 1
            , Border.dotted
            , Color.border Color.grey
            ]
        ]
