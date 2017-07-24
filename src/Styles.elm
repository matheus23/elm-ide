module Styles exposing (..)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type alias Variations =
    Never


type Styles
    = NoStyle
    | Keyword
    | Identifier
    | TypeHole
    | TypeHoleText
    | Draggable
    | Droppable
    | Dragged
    | DragHover


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style NoStyle
            []

        -- Text styles
        , style Keyword
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.lightBlue
            ]
        , style Identifier
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text (Color.rgb 30 30 30)
            ]
        , style TypeHoleText
            [ Font.typeface [ "Lucida Console", "Courier New" ]
            , Color.text Color.darkGrey
            ]
        , style TypeHole
            [ Border.all 1
            , Color.border Color.darkGrey
            ]

        -- Drag Drop styles
        , style Draggable
            [ Color.border (Color.rgba 0 0 0 0)
            , Border.all 1
            ]
        , style Droppable
            [ Border.all 1
            , Border.dotted
            , Color.border Color.grey
            ]
        , style Dragged
            [ Color.text Color.grey
            , Border.all 1
            , Color.border (Color.rgba 0 0 0 0)
            ]
        , style DragHover
            [ Border.all 1
            ]
        ]
