module Main exposing (main)

import Browser exposing (element)
import Html as H
import Html.Attributes as HA
import List exposing (filter, head, length, map, map2, member, tail)
import List.Extra exposing (andThen, minimumBy)
import String exposing (join)
import Svg exposing (g, line, rect, svg)
import Svg.Attributes exposing (fill, height, style, version, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)
import Time exposing (every)
import Tuple


cell_size : Int -> Int
cell_size value =
    value * 25



type alias Cell =
    ( Int, Int )

type alias BoardSize =
    ( Int, Int )

board_width: BoardSize -> Int
board_width bs =
    Tuple.second bs

board_height: BoardSize -> Int
board_height bs =
    Tuple.first bs

decrement_size: Int -> Int
decrement_size n =
    let
        minimum_size = 3
    in
        if n <= minimum_size then
            minimum_size
        else
            n - 1
increment_size: Int -> Int
increment_size n =
    let
        maximum_size = 40
    in
        if n >= maximum_size then
            maximum_size
        else
            n + 1

increment_pause: Float -> Float
increment_pause n =
    n + 1

-- decreasing pause time (ms) increases speed
decrement_pause: Float -> Float
decrement_pause n =
    let
        minimum_pause = 1
    in
        if n <= minimum_pause then
            minimum_pause
        else
            n - 1

type alias Model =
    { path : List Cell
    , board : List Cell
    , pause_ms : Float
    , size : BoardSize
    }


type Msg
    = Tick Time.Posix
    | SetStart Cell
    | SetSize BoardSize
    | SetPause Float


initialize_board : BoardSize -> List Cell
initialize_board board_size =
            List.range 0 (Tuple.first board_size - 1)
                |> andThen
                    (\r ->
                        List.range 0 (Tuple.second board_size - 1)
                            |> andThen
                                (\c ->
                                    [ ( r, c ) ]
                                )
                    )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        -- Initial board height and width
        initial_size =
            8

        -- Initial chess board
        initial_board =
            initialize_board (initial_size, initial_size)

        initial_path =
            []
        initial_pause =
            10
    in
    ( Model initial_path initial_board initial_pause (initial_size, initial_size), Cmd.none )


view : Model -> H.Html Msg
view model =
    let
        showChecker row col =
            rect
                [ x <| String.fromInt col
                , y <| String.fromInt row
                , width "1"
                , height "1"
                , fill <|
                    if modBy 2 (row + col) == 0 then
                        "blue"

                    else
                        "grey"
                , onClick <| SetStart ( row, col )
                ]
                []

        showMove ( row0, col0 ) ( row1, col1 ) =
            line
                [ x1 <| String.fromFloat (toFloat col0 + 0.5)
                , y1 <| String.fromFloat (toFloat row0 + 0.5)
                , x2 <| String.fromFloat (toFloat col1 + 0.5)
                , y2 <| String.fromFloat (toFloat row1 + 0.5)
                , style "stroke:yellow;stroke-width:0.05"
                ]
                []

        render mdl =
            let
                checkers =
                    mdl.board
                        |> andThen
                            (\( r, c ) ->
                                [ showChecker r c ]
                            )

                moves =
                    case List.tail mdl.path of
                        Nothing ->
                            []

                        Just tl ->
                            List.map2 showMove mdl.path tl
            in
            checkers ++ moves

        unvisited =
            length model.board - length model.path

        center =
            [ HA.style "text-align" "center" ]

        table =
            [ HA.style "text-align" "center", HA.style "display" "table", HA.style "width" "auto", HA.style "margin" "auto" ]
        table_row =
            [ HA.style "display" "table-row", HA.style "width" "auto" ]

        table_cell =
            [ HA.style "display" "table-cell", HA.style "width" "auto", HA.style "padding" "1px 3px" ]
        rows =
            board_height model.size

        cols =
            board_width model.size
    in
    H.div
        []
        [ H.h1 center [ H.text "Knight's Tour" ]
        , H.div
            table
            [ H.div
                table_row
                [ H.div
                    table_cell
                    [ H.text "Rows"]
                , H.div
                    table_cell
                    [ H.text "Columns"]
                , H.div
                    table_cell
                    [ H.text ""]
                , H.div
                    table_cell
                    [ H.text "Pause (ms)"]
                ]
            , H.div
                table_row
                [ H.div
                    table_cell
                    [ H.button [onClick <| SetSize ( increment_size rows, cols )] [ H.text "▲"] ]
                , H.div
                    table_cell
                    [ H.button [onClick <| SetSize ( rows, increment_size cols )] [ H.text "▲"] ]
                , H.div
                    table_cell
                    [ H.text ""]
                , H.div
                    table_cell
                    [ H.button [onClick <| SetPause ( increment_pause model.pause_ms )] [ H.text "▲"] ]
                ]
            , H.div
                table_row
                [ H.div
                    table_cell
                    [ H.text <| String.fromInt rows ]
                , H.div
                    table_cell
                    [ H.text <| String.fromInt cols]
                , H.div
                    table_cell
                    [ H.text ""]
                , H.div
                    table_cell
                    [ H.text <| String.fromFloat model.pause_ms]
                ]
            , H.div
                table_row
                [ H.div
                    table_cell
                    [ H.button [onClick <| SetSize ( decrement_size rows, cols )] [ H.text "▼"] ]
                , H.div
                    table_cell
                    [ H.button [onClick <| SetSize ( rows, decrement_size cols )] [ H.text "▼"] ]
                , H.div
                    table_cell
                    [ H.text ""]
                , H.div
                    table_cell
                    [ H.button [onClick <| SetPause ( decrement_pause model.pause_ms )] [ H.text "▼"] ]
                ]
            ]
        , H.h2 center [ H.text "(pick a square)" ]
        , H.div
            center
            [ svg
                [ version "1.1"
                , width (String.fromInt (cell_size cols))
                , height (String.fromInt (cell_size rows))
                , viewBox
                    (join " "
                        [ String.fromInt 0
                        , String.fromInt 0
                        , String.fromInt cols
                        , String.fromInt rows
                        ]
                    )
                ]
                [ g [] <| render model ]
            ]
        , H.h3 center [ H.text <| "Unvisited count : " ++ String.fromInt unvisited ]
        ]


nextMoves : Model -> Cell -> List Cell
nextMoves model ( stRow, stCol ) =
    let
        c =
            [ 1, 2, -1, -2 ]

        km =
            c
                |> andThen
                    (\cRow ->
                        c
                            |> andThen
                                (\cCol ->
                                    if abs cRow == abs cCol then
                                        []

                                    else
                                        [ ( cRow, cCol ) ]
                                )
                    )

        jumps =
            List.map (\( kmRow, kmCol ) -> ( kmRow + stRow, kmCol + stCol )) km
    in
    List.filter (\j -> List.member j model.board && not (List.member j model.path)) jumps


bestMove : Model -> Maybe Cell
bestMove model =
    case List.head model.path of
        Just mph ->
            minimumBy (List.length << nextMoves model) (nextMoves model mph)
        _ ->
            Nothing

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mo =
            case msg of
                SetStart start ->
                    { model | path = [ start ] }

                Tick _ ->
                    case model.path of
                        [] ->
                            model

                        _ ->
                            case bestMove model of
                                Nothing ->
                                    model

                                Just best ->
                                    { model | path = best :: model.path }
                SetSize board_size ->
                    { model | board = initialize_board board_size, path = [], size = board_size }
                SetPause pause ->
                    { model | pause_ms = pause }
    in
    ( mo, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.pause_ms Tick

main: Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
