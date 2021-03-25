module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)


type alias Model =
    { parenthesis : String }


type Parenthesis
    = Left
    | Right


initialModel : Model
initialModel =
    { parenthesis = "()" }


type Msg
    = Change String


charToParenthesis : Char -> Maybe Parenthesis
charToParenthesis c =
    case c of
        '(' ->
            Just Left

        ')' ->
            Just Right

        _ ->
            Nothing


openParens : String -> Maybe Int
openParens str =
    str
        |> String.toList
        |> List.filterMap charToParenthesis
        |> List.foldl
            (\p count ->
                case p of
                    Left ->
                        count
                            |> Maybe.map (\n -> n + 1)

                    Right ->
                        count
                            |> Maybe.andThen
                                (\n ->
                                    if n == 0 then
                                        Nothing

                                    else
                                        Just (n - 1)
                                )
            )
            (Just 0)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change str ->
            { model | parenthesis = str }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                (if openParens model.parenthesis == Just 0 then
                    "Valid"

                 else
                    "Invalid"
                )
            ]
        , input
            [ class "form-control"
            , placeholder "type a series of ()"
            , value model.parenthesis
            , onInput Change
            ]
            []
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
