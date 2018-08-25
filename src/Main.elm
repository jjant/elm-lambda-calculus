module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Lambda exposing (Expr(..), Lit(..))
import Eval exposing (getSteps)
import CmdLine exposing (..)
import Util exposing (takeLast)


terminalWindow =
    [ ( "margin", "0 auto" )
    , ( "width", "600px" )
    , ( "text-align", "left" )
    , ( "postion", "relative" )
    , ( "border-radius", "10px" )
    , ( "background-color", "#0D1F2D" )
    , ( "color", "#F4FAFF" )
    , ( "font-size", "11pt" )
    ]


terminalWindowBody =
    [ ( "padding-top", "10px" )
    , ( "height", "400px" )
    ]


terminalWindowHeader =
    [ ( "background-color", "#E0E8F0" )
    , ( "height", "30px" )
    , ( "border-radius", "8px 8px 0 0" )
    , ( "padding-left", "10px" )
    ]


terminalWindowButton =
    [ ( "width", "12px" )
    , ( "height", "12px" )
    , ( "margin", "10px 4px 0 0" )
    , ( "display", "inline-block" )
    , ( "border-radius", "8px" )
    ]


redBtn =
    [ ( "background-color", "#E75448" ) ]


greenBtn =
    [ ( "background-color", "#3BB662" ) ]


yellowBtn =
    [ ( "background-color", "#E5C30F" ) ]


terminal_text =
    [ ( "margin-left", "16px" )
    , ( "font-family", "Menlo, Monaco, Consolas, Courier New, Courier" )
    ]


typedStrings =
    [ ( "display", "inline-block" )
    , ( "postiion", "relative" )
    ]


typed =
    [ ( "font-family", "Menlo, Monaco, Consolas, Courier New, Courier" )
    , ( "margin-left", "15px" )
    ]


inputStyle =
    [ ( "background", "transparent" )
    , ( "border", "none" )
    , ( "color", "#F4FAFF" )
    , ( "font-size", "15px" )
    ]


oldStyle : List ( String, String ) -> List (Html.Attribute a)
oldStyle =
    List.map (uncurry style)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


val =
    App (Lam "x" (Var "x")) (Lit (LInt 3))



-----


type alias Model =
    { textInput : String, history : List String }


init =
    ( { textInput = "", history = [] }, Cmd.none )


type Msg
    = ChangeText String
    | KeyPressed String
    | Delete
    | Submit
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText text ->
            ( { model | textInput = model.textInput ++ " " ++ text }, Cmd.none )

        KeyPressed t ->
            ( { model | textInput = model.textInput ++ t }, Cmd.none )

        Delete ->
            ( { model | textInput = String.dropRight 1 model.textInput }, Cmd.none )

        Submit ->
            ( { model | history = model.history ++ [ model.textInput ], textInput = "" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


renderResult : String -> List (Html a)
renderResult code =
    let
        renderStep step =
            div [] [ text <| " => " ++ step ]
    in
        div [] [ text ("> " ++ code) ] :: (List.map renderStep (process code))



-- div [] [ text ("> " ++ code) ] :: (List.map (renderStep << Debug.toString) (parseAndSteps code))


view model =
    let
        styles =
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "margin-top", "60px" )
            ]
    in
        div (oldStyle styles)
            [ terminal model
            , examples
            ]


terminal { textInput, history } =
    div (oldStyle terminalWindow)
        [ header (oldStyle terminalWindowHeader) [ redButton, greenButton, yellowButton ]
        , div (oldStyle terminalWindowBody)
            [ div (oldStyle (terminal_text ++ typedStrings))
                (takeLast 21 (List.concatMap renderResult history) ++ [ text ("> " ++ textInput) ])
            ]
        ]


redButton =
    div (oldStyle (redBtn ++ terminalWindowButton)) []


greenButton =
    div (oldStyle (greenBtn ++ terminalWindowButton)) []


yellowButton =
    div (oldStyle (yellowBtn ++ terminalWindowButton)) []


example title codeText =
    div ([ onClick (ChangeText codeText) ] ++ oldStyle exampleStyle)
        [ span [] [ text title ]
        , pre [] [ code [] [ text codeText ] ]
        ]


examples =
    div (oldStyle examplesStyle)
        [ div [] [ text "Try some examples!" ]
        , div []
            [ text "Booleans"
            , div []
                [ example "true" "(\\x y. x)"
                , example "false" "(\\x y. y)"
                , example "if then else" "(\\b t e. b t e)"
                ]
            ]
        , div []
            [ text "Numbers"
            , div []
                [ example "zero" "(\\f x. x)"
                , example "one" "(\\f x. f x)"
                , example "two" "(\\f x. f (f x))"
                , example "succ" "(\\n f x. f (n f x))"
                , example "plus" "(\\m n f x. m f (n f x))"
                ]
            ]
        ]


exampleStyle =
    [ ( "background", "lightgray" )
    ]


examplesStyle =
    [ ( "background", "#ABA9BF" )
    , ( "margin", "0 auto" )
    , ( "width", "800px" )
    ]



------


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Key
    = Character Char
    | Control String


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map (toMsg << toKey)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


toMsg : Key -> Msg
toMsg key =
    case key of
        Character c ->
            KeyPressed (String.fromChar c)

        Control c ->
            case c of
                "Enter" ->
                    Submit

                "Backspace" ->
                    Delete

                _ ->
                    NoOp
