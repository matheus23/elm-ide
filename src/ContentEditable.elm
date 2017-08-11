module ContentEditable exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (contenteditable, property)
import Element.Events exposing (on, onBlur, onFocus)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { liveContent : String
    , content : String
    , focused : Bool
    }


type Msg
    = UserInput String
    | Blur
    | Focus


create : String -> Model
create content =
    { content = content, liveContent = content, focused = False }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserInput newText ->
            { model | liveContent = newText }

        Blur ->
            { model
                | content = model.liveContent
                , focused = False
            }

        Focus ->
            { model
                | focused = True
            }


view : style -> Model -> Element style variation Msg
view style =
    viewAttr [] style True


viewAttr : List (Element.Attribute variation Msg) -> style -> Bool -> Model -> Element style variation Msg
viewAttr attributes style active model =
    Element.el style
        ([ contenteditable (active || model.focused)
         , on "input" (Decode.map UserInput innerHtmlDecoder)
         , onBlur Blur
         , onFocus Focus

         -- placing this here, instead of using Html.text
         -- fixes a "cannot read property 'replaceData' of undefined"-error
         , property "innerText" (Encode.string model.content)
         ]
            ++ attributes
        )
        Element.empty


innerHtmlDecoder : Decode.Decoder String
innerHtmlDecoder =
    Decode.at [ "target", "innerHTML" ] Decode.string
