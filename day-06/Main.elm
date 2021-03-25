module Main exposing (main)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \{ running } ->
                if running then
                    Time.every 500 Tick

                else
                    Sub.none
        }



-- Model


type Living
    = Alive
    | Dead


type alias Cell =
    { st : Living
    }


type alias Model =
    { running : Bool
    , matrix : Matrix Cell
    }


initialModel : Model
initialModel =
    { running = False
    , matrix =
        Matrix.repeat ( 10, 10 )
            (Cell Dead)
    }



-- Update


type Msg
    = Swap ( Int, Int )
    | RequestedUpdateRunning Bool
    | Tick Time.Posix


update : Msg -> Model -> Model
update msg ({ matrix } as model) =
    case msg of
        Swap ( x, y ) ->
            { model
                | matrix =
                    matrix
                        |> matrixUpdate ( x, y )
                            (Maybe.map swap >> Maybe.withDefault (Cell Dead))
            }

        RequestedUpdateRunning isRunning ->
            { model | running = isRunning }

        Tick _ ->
            { model | matrix = conway matrix }


matrixUpdate : ( Int, Int ) -> (Maybe a -> a) -> Matrix a -> Matrix a
matrixUpdate coord f matrix =
    matrix
        |> Matrix.set coord
            (f (Matrix.get coord matrix))


swap : Cell -> Cell
swap c =
    case c.st of
        Dead ->
            { c | st = Alive }

        Alive ->
            { c | st = Dead }


conway : Matrix Cell -> Matrix Cell
conway matrix =
    matrix
        |> Matrix.indexedMap
            (\coord c ->
                let
                    livingNeighbours =
                        numberLivingNeighbours coord matrix
                in
                case c.st of
                    Dead ->
                        if livingNeighbours == 3 then
                            { st = Alive }

                        else
                            { st = Dead }

                    Alive ->
                        if livingNeighbours == 2 || livingNeighbours == 3 then
                            { st = Alive }

                        else
                            { st = Dead }
            )


numberLivingNeighbours : ( Int, Int ) -> Matrix Cell -> Int
numberLivingNeighbours ( x, y ) matrix =
    matrix
        |> Matrix.toIndexedList
        |> List.filter
            (\( ( otherX, otherY ), cell ) ->
                let
                    deltaX =
                        x - otherX

                    deltaY =
                        y - otherY
                in
                (cell.st == Alive)
                    && not (x == otherX && y == otherY)
                    && (-1 <= deltaX && deltaX <= 1)
                    && (-1 <= deltaY && deltaY <= 1)
            )
        |> List.length



-- View


view : Model -> Html Msg
view { matrix, running } =
    div []
        [ matrixView
            matrix
        , button
            [ onClick (RequestedUpdateRunning False)
            , disabled (not running)
            ]
            [ text "Stop" ]
        , button
            [ onClick (RequestedUpdateRunning True)
            , disabled running
            ]
            [ text "Start" ]
        ]


matrixView : Matrix Cell -> Html Msg
matrixView matrix =
    table []
        (matrix
            |> Matrix.toList
            |> List.indexedMap
                (\y row ->
                    tr []
                        (row
                            |> List.indexedMap
                                (\x cell ->
                                    cellView ( x, y ) cell
                                )
                        )
                )
        )


cellView : ( Int, Int ) -> Cell -> Html Msg
cellView coord cell =
    td
        [ style "border" "1px black solid"
        , style "width" "25px"
        , style "height" "25px"
        , style "background-color" (livingToColor cell.st)
        , onClick (Swap coord)
        ]
        []


livingToColor : Living -> String
livingToColor l =
    case l of
        Dead ->
            "white"

        Alive ->
            "black"