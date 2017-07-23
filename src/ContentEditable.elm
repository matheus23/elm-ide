module ContentEditable exposing (..)

import Html
import Html.Attributes exposing (contenteditable, property)
import Html.Events exposing (on, onBlur, onFocus)
import Json.Decode as Decode
import Json.Encode as Encode
import Util


type alias Model =
    { liveContent : String
    , content : String
    }


type Msg
    = UserInput String
    | Blur


create : String -> Model
create content =
    { content = content, liveContent = content }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserInput newText ->
            { model | liveContent = newText }

        Blur ->
            { model | content = model.liveContent }


view : List (Html.Attribute Msg) -> Model -> Html.Html Msg
view styles model =
    Html.div
        ([ contenteditable True
         , on "input" (Decode.map UserInput innerHtmlDecoder)
         , onBlur Blur

         -- placing this here, instead of using Html.text
         -- fixes a "cannot read property 'replaceData' of undefined"-error
         , property "innerHTML" (Encode.string model.content)
         ]
            ++ styles
        )
        []


tightView : Model -> Html.Html Model
tightView =
    Util.tightHtml update (view [])


innerHtmlDecoder : Decode.Decoder String
innerHtmlDecoder =
    Decode.at [ "target", "innerHTML" ] Decode.string
