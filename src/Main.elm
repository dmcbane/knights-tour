module Main exposing (main)

import Browser exposing (element)
import Html as H
import Html.Attributes as HA
import List exposing (concat, concatMap, filter, foldl, head, length, map, map2, member, minimum, tail)
import List.Extra exposing (andThen, minimumBy)
import String exposing (join)
import Svg exposing (g, line, rect, svg)
import Svg.Attributes exposing (fill, height, style, version, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)
import Time exposing (Posix, every)


w =
    450


h =
    450


rowCount =
    20


colCount =
    20


type alias Cell =
    ( Int, Int )


type alias Model =
    { path : List Cell
    , board : List Cell
    }


type Msg
    = NoOp
    | Tick Time.Posix
    | SetStart Cell


init : () -> ( Model, Cmd Msg )
init _ =
    let
        board =
            List.range 0 (rowCount - 1)
                |> andThen
                    (\r ->
                        List.range 0 (colCount - 1)
                            |> andThen
                                (\c ->
                                    [ ( r, c ) ]
                                )
                    )

        path =
            []
    in
    ( Model path board, Cmd.none )


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
    in
    H.div
        []
        [ H.h2 center [ H.text "Knight's Tour" ]
        , H.h2 center [ H.text <| "Unvisited count : " ++ String.fromInt unvisited ]
        , H.h2 center [ H.text "(pick a square)" ]
        , H.div
            center
            [ svg
                [ version "1.1"
                , width (String.fromInt w)
                , height (String.fromInt h)
                , viewBox
                    (join " "
                        [ String.fromInt 0
                        , String.fromInt 0
                        , String.fromInt colCount
                        , String.fromInt rowCount
                        ]
                    )
                ]
                [ g [] <| render model ]
            ]
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
        Nothing ->
            Nothing

        Just mph ->
            minimumBy (List.length << nextMoves model) (nextMoves model mph)


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

                NoOp ->
                    model
    in
    ( mo, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 Tick


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
