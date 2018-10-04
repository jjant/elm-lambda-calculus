module Editor exposing (view, unattr, id, value, onChange, readOnly)

import Html.Attributes as Attributes exposing (property)
import Html.Events exposing (on)
import Html exposing (Html, Attribute)
import Json.Encode as Encode
import Json.Decode as Decode


type Attribute msg
    = Attr (Html.Attribute msg)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Attr <| on "change" (Decode.map tagger (Decode.at [ "target", "editorValue" ] Decode.string))


unattr : Attribute msg -> Html.Attribute msg
unattr (Attr a) =
    a


id : String -> Attribute msg
id =
    Attributes.id >> Attr


value : String -> Attribute msg
value =
    Encode.string >> property "editorValue" >> Attr


readOnly : Attribute msg
readOnly =
    Attr <| property "readOnly" <| Encode.bool True


view : List (Attribute msg) -> Html msg
view attributes =
    Html.node "code-editor" (List.map unattr attributes) []
