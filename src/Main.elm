module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Lambda exposing (Expr(..), Lit(..))
import Eval exposing (getSteps)


-- import CmdLine exposing (runEval, process, prettySteps)

import CmdLine exposing (..)
import Util exposing (takeLast)


terminalWindow =
    [ ( "margin", "0 auto" )
    , ( "width", "600px" )
    , ( "height", "400px" )
    , ( "tex-align", "left" )
    , ( "margin-top", "15px" )
    , ( "postion", "relative" )
    , ( "border-radius", "10px" )
    , ( "background-color", "#0D1F2D" )
    , ( "color", "#F4FAFF" )
    , ( "font-size", "11pt" )
    ]


terminal_window_header =
    [ ( "background-color", "#E0E8F0" )
    , ( "height", "30px" )
    , ( "border-radius", "8px 8px 0 0" )
    , ( "padding-left", "10px" )
    ]


terminal_window_button =
    [ ( "width", "12px" )
    , ( "height", "12px" )
    , ( "margin", "10px 4px 0 0" )
    , ( "display", "inline-block" )
    , ( "border-radius", "8px" )
    ]


red_btn =
    [ ( "background-color", "#E75448" ) ]


green_btn =
    [ ( "background-color", "#3BB662" ) ]


yellow_btn =
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
            ( { model | textInput = text }, Cmd.none )

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


view model =
    div []
        [ terminal model
        , examples
        ]


terminal { textInput, history } =
    div (oldStyle terminalWindow)
        [ header (oldStyle terminal_window_header)
            [ div (oldStyle (red_btn ++ terminal_window_button))
                []
            , div (oldStyle (green_btn ++ terminal_window_button))
                []
            , div (oldStyle (yellow_btn ++ terminal_window_button))
                []
            ]
        , div (oldStyle terminalWindow)
            [ div
                (oldStyle (terminal_text ++ typedStrings))
                (takeLast 21 (List.concatMap renderResult history) ++ [ text ("> " ++ textInput) ])
            ]
        ]


example title codeText =
    div [ onClick (ChangeText codeText) ]
        [ span [] [ text title ]
        , pre [] [ code [] [ text codeText ] ]
        ]


examples =
    div []
        [ div [] [ text "Try some examples!" ]
        , example "true" "\\x y. x"
        , example "false" "\\x y. y"
        , example "if then else" "\\b t e. b t e"
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
