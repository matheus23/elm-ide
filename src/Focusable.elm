module Focusable exposing (..)

import Element as Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Events as Events
import Focus exposing (..)
import FocusMore as Focus exposing (FieldSetter)
import Util


-- Model


type alias Model base =
    { base
        | focused : Bool
    }


type Msg
    = SetFocus Bool



-- Update


update : Msg -> Model base -> Model base
update (SetFocus focus) model =
    model & focused .= focus



-- View


wrapFocusable : style -> (Msg -> msg) -> Element style variation msg -> Element style variation msg
wrapFocusable style injectMsg element =
    Element.el style (attributes injectMsg) element


attributes : (Msg -> msg) -> List (Element.Attribute variation msg)
attributes injectMsg =
    [ Events.onBlur (injectMsg (SetFocus False))
    , Events.onFocus (injectMsg (SetFocus True))
    , tabindex 0
    ]



-- Lenses


focused : FieldSetter (Model a) Bool
focused f model =
    { model | focused = f model.focused }
