module Styles exposing (..)

import Style exposing (..)
import Style.Border as Border
import Style.Font as Font


type alias Variations =
    Never


type Styles
    = StdStyle
    | TextArea
    | VarInput


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style TextArea
            [ Font.typeface [ "Lucida Console" ]
            , Border.all 1
            ]
        , style VarInput
            [ Font.typeface [ "Lucida Console" ] ]
        ]
